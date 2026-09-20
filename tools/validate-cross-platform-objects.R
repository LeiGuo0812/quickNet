#!/usr/bin/env Rscript
# Consume real Windows objects in fresh Linux processes, comparing refits with
# the corresponding historical quickNet source running on the Linux backends.
# Usage: Rscript --vanilla tools/validate-cross-platform-objects.R [input] [output]
# Inputs are produced by tools/validate-legacy-objects.R on Windows.
args <- commandArgs(trailingOnly = TRUE)
script_arg <- commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][[1L]]
script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
options(mc.cores = 1L)
`%||%` <- function(x, y) if (is.null(x)) y else x
versions <- function() {
  packages <- c("bootnet", "qgraph", "IsingFit", "mgm", "glmnet", "Matrix")
  as.list(c(R = as.character(getRversion()), platform = R.version$platform,
    vapply(packages, function(x) as.character(utils::packageVersion(x)), "")))
}

# These are the exact constructor arguments in validate-legacy-objects.R.
# Data are read from the Windows oracle, never regenerated using a new RNG.
constructor_spec <- function(label, name, data) {
  common <- list(pie = FALSE, DoNotPlot = TRUE, labels = LETTERS[1:4],
                 groups = list(one = 1:2, two = 3:4))
  spec <- switch(name,
    EBICglasso = list(data = data, model = "EBICglasso", cor_method = "spearman"),
    EBIC_missing_none = list(data = data, model = "EBICglasso",
      missing = if (label == "current") "pairwise" else "none"),
    ising = list(data = data, model = "ising", AND = FALSE),
    mgm = list(data = data, model = "mgm", types = rep("g", 4), levels = rep(1, 4)),
    partial = list(data = data, model = "partial", cor_method = "spearman"),
    clpn = list(data = data, nodes = LETTERS[1:3], waves = 1:3, standardize = TRUE,
      alpha = .6, lambda_rule = "lambda.min", nfolds = 4, seed = 703),
    stop("Unknown fixture case: ", name))
  if (label == "current") {
    if (name == "EBICglasso") spec <- c(spec, list(gamma = .17, nlambda = 40))
    if (name == "mgm") spec <- c(spec, list(lambdaSel = "EBIC", ruleReg = "OR", gamma = .13))
    if (name == "clpn") spec <- c(spec, list(standardize_data = TRUE, intercept = FALSE))
  }
  if (name != "clpn") spec <- c(spec, common)
  spec
}

make_reference <- function(source, input, output, label) {
  pkgload::load_all(source, quiet = TRUE)
  manifest <- readRDS(file.path(input, "manifest.rds"))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(file.path(output, "linux-reference-plots.pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (name in names(manifest$cases)) {
    oracle <- readRDS(file.path(input, "objects", paste0(name, "-oracle.rds")))
    spec <- constructor_spec(label, name, oracle$data)
    set.seed(918)
    fit <- do.call(if (name == "clpn") PanelNet else quickNet, spec)
    spec$data <- NULL
    saveRDS(list(graph = fit$graph, networks = fit$networks, meta = fit$meta,
      constructor_arguments = spec, versions = versions()),
      file.path(output, paste0(name, "-linux-reference.rds")), version = 3)
    message("Linux historical-source reference: ", label, "/", name)
  }
}

consume <- function(input, output, label) {
  pkgload::load_all(root, quiet = TRUE)
  manifest <- readRDS(file.path(input, "manifest.rds"))
  check <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
  eq <- function(x, y, message, tolerance = 1e-8) {
    check(isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE)), message)
  }
  grDevices::pdf(file.path(output, "linux-consumed-plots.pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  results <- list(label = label, created_with = manifest$versions,
                  read_with = versions(), cases = list())
  for (name in names(manifest$cases)) {
    warnings <- character()
    result <- tryCatch(withCallingHandlers({
      fit <- readRDS(file.path(input, "objects", manifest$cases[[name]]$file))
      oracle <- readRDS(file.path(input, "objects", paste0(name, "-oracle.rds")))
      reference <- readRDS(file.path(output, paste0(name, "-linux-reference.rds")))
      graph_before <- fit$graph
      networks_before <- fit$networks
      check(inherits(fit, "quicknet_fit"), "Object class changed after reading")
      check(length(capture.output(print(fit))) > 0, "print failed")
      check(is.data.frame(summary(fit)), "summary failed")
      check(inherits(quicknet_report(fit), "quicknet_report"), "report failed")
      check(inherits(plot(fit), "qgraph"), "plot failed")
      check(identical(graph_before, fit$graph) && identical(networks_before, fit$networks),
            "Display changed the stored graph")
      roundtrip <- tempfile(fileext = ".rds")
      saveRDS(fit, roundtrip, version = 3)
      check(identical(readRDS(roundtrip)$graph, graph_before), "Graph changed in RDS roundtrip")
      unlink(roundtrip)

      if (name == "clpn") {
        meta <- fit$meta
        refit <- quickNet:::quicknet_refit_with_backend_args(PanelNet, fit,
          data = oracle$data, nodes = meta$nodes, waves = meta$waves, id = meta$id,
          prefix = meta$prefix, standardize = meta$standardize,
          standardize_data = meta$standardize_data %||% FALSE,
          alpha = meta$alpha, lambda_rule = meta$lambda_rule, nfolds = meta$nfolds,
          seed = meta$seed)
      } else {
        set.seed(918)
        refit <- quickNet:::quicknet_refit_like(oracle$data, fit)
        settings <- quickNet:::quicknet_cross_refit_args(fit)
        eq(quickNet:::quicknet_nct_fit_settings(fit), settings, "NCT settings differ")
        set.seed(918)
        nct <- quickNet:::quicknet_nct_refit(oracle$data, settings)
        eq(nct, reference$graph, "NCT refit differs from Linux historical constructor")
      }
      eq(refit$graph, reference$graph, "Recovered refit differs from Linux historical constructor")
      check(identical(names(refit$networks), names(reference$networks)), "Network layer names changed")
      for (layer in names(reference$networks))
        eq(refit$networks[[layer]], reference$networks[[layer]], paste("Refitted layer differs:", layer))
      list(status = "passed", graph_preserved = TRUE,
        linux_reference_max_abs_difference = max(abs(refit$graph - reference$graph)),
        windows_oracle_max_abs_difference = max(abs(refit$graph - oracle$graph)),
        refit_metadata = refit$meta[c("gamma", "cor_method", "missing", "AND", "standardize", "standardize_data")])
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) list(status = "failed", error = conditionMessage(e)))
    result$warnings <- unique(warnings)
    results$cases[[name]] <- result
    message("Cross-platform validation ", label, "/", name, ": ", result$status,
      if (!is.null(result$error)) paste0(" - ", result$error) else "")
  }
  saveRDS(results, file.path(output, "validation.rds"), version = 3)
  jsonlite::write_json(results, file.path(output, "validation.json"), pretty = TRUE,
                       auto_unbox = TRUE, null = "null")
  if (any(vapply(results$cases, function(x) x$status != "passed", logical(1)))) quit(status = 1L)
}

if (length(args) && args[[1L]] == "--reference") {
  make_reference(args[[2L]], args[[3L]], args[[4L]], args[[5L]])
} else if (length(args) && args[[1L]] == "--consume") {
  consume(args[[2L]], args[[3L]], args[[4L]])
} else {
  input <- if (length(args)) args[[1L]] else file.path(root, "..", "output", "audit", "legacy-objects-windows")
  output <- if (length(args) >= 2L) args[[2L]] else file.path(root, "..", "output", "audit", "cross-platform-objects")
  input <- normalizePath(input, mustWork = TRUE)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output)
  run <- function(arguments, log) {
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script), shQuote(arguments)), stdout = log, stderr = log)
    if (status != 0) stop("Subprocess failed; inspect ", log, call. = FALSE)
  }
  for (label in c("29a414b", "72666a3", "current")) {
    source <- root
    case_input <- file.path(input, label)
    case_output <- file.path(output, label)
    check_manifest <- file.path(case_input, "manifest.rds")
    if (!file.exists(check_manifest)) stop("Missing Windows fixture manifest: ", check_manifest)
    dir.create(case_output, recursive = TRUE, showWarnings = FALSE)
    if (label != "current") {
      source <- file.path(case_output, "source")
      dir.create(source, recursive = TRUE, showWarnings = FALSE)
      archive <- file.path(case_output, "source.tar")
      commit <- system2("git", c("-C", shQuote(root), "rev-parse", label), stdout = TRUE)
      status <- system2("git", c("-C", shQuote(root), "archive", "--format=tar",
        paste0("--output=", shQuote(archive)), commit))
      if (status != 0L) stop("git archive failed")
      utils::untar(archive, exdir = source)
      writeLines(commit, file.path(case_output, "source-commit.txt"))
    }
    run(c("--reference", source, case_input, case_output, label), file.path(case_output, "reference.log"))
    run(c("--consume", case_input, case_output, label), file.path(case_output, "consume.log"))
    message("Completed cross-platform validation: ", label)
  }
}
