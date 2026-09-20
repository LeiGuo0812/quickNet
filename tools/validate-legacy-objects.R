#!/usr/bin/env Rscript
# Rebuild real objects with fixed historical source snapshots, serialize them,
# then validate them in a separate process running the current source.
# Usage: Rscript --vanilla tools/validate-legacy-objects.R [output-directory]

args <- commandArgs(trailingOnly = TRUE)
script_arg <- commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][[1L]]
script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
options(mc.cores = 1L)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

versions <- function() {
  packages <- c("bootnet", "qgraph", "IsingFit", "mgm", "glmnet", "Matrix")
  as.list(c(R = as.character(getRversion()), vapply(packages, function(x) as.character(utils::packageVersion(x)), "")))
}

make_data <- function() {
  set.seed(60403)
  z <- matrix(rnorm(160 * 4), 160, 4)
  x <- cbind(A = exp(.3 * z[, 1]), B = .8 * z[, 1] + .6 * z[, 2],
             C = -.55 * z[, 2] + .7 * z[, 3], D = .4 * z[, 1] + .8 * z[, 4])
  latent <- rnorm(200)
  binary <- vapply(c(-.3, .1, .3, -.1), function(a) rbinom(200, 1, plogis(a + 1.5 * latent)), numeric(200))
  colnames(binary) <- LETTERS[1:4]
  panel <- data.frame(id = seq_len(100))
  wave <- matrix(rnorm(300), 100, 3) %*% diag(c(.6, 2, 5))
  for (j in 1:3) {
    if (j > 1) wave <- wave %*% matrix(c(.65, -.15, .05, .2, .5, .1, .05, -.1, .55), 3) + matrix(rnorm(300), 100, 3)
    for (k in 1:3) panel[[paste0(LETTERS[k], "_t", j)]] <- wave[, k] + j * k
  }
  list(gaussian = as.data.frame(x), binary = as.data.frame(binary), panel = panel)
}

create_objects <- function(source, output, label) {
  pkgload::load_all(source, quiet = TRUE)
  dir.create(file.path(output, "objects"), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(file.path(output, "creation-plots.pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  dat <- make_data()
  incomplete <- dat$gaussian
  incomplete[cbind(c(2, 7, 16, 29), c(1, 2, 3, 4))] <- NA_real_
  common <- list(pie = FALSE, DoNotPlot = TRUE, labels = LETTERS[1:4], groups = list(one = 1:2, two = 3:4))
  designs <- list(
    EBICglasso = list(data = dat$gaussian, model = "EBICglasso", cor_method = "spearman"),
    EBIC_missing_none = list(data = incomplete, model = "EBICglasso", missing = if (label == "current") "pairwise" else "none"),
    ising = list(data = dat$binary, model = "ising", AND = FALSE),
    mgm = list(data = dat$gaussian, model = "mgm", types = rep("g", 4), levels = rep(1, 4)),
    partial = list(data = dat$gaussian, model = "partial", cor_method = "spearman"),
    clpn = list(data = dat$panel, nodes = LETTERS[1:3], waves = 1:3, standardize = TRUE,
                alpha = .6, lambda_rule = "lambda.min", nfolds = 4, seed = 703)
  )
  if (label == "current") {
    designs$EBICglasso$gamma <- .17
    designs$EBICglasso$nlambda <- 40
    designs$mgm$lambdaSel <- "EBIC"
    designs$mgm$ruleReg <- "OR"
    designs$mgm$gamma <- .13
    designs$clpn$standardize_data <- TRUE
    designs$clpn$intercept <- FALSE
  }
  manifest <- list(label = label, versions = versions(), source = normalizePath(source), cases = list())
  for (name in names(designs)) {
    spec <- designs[[name]]
    fun <- if (name == "clpn") PanelNet else quickNet
    if (name != "clpn") spec <- c(spec, common)
    set.seed(918)
    fit <- do.call(fun, spec)
    set.seed(2408)
    sampled <- sample.int(nrow(spec$data), nrow(spec$data), replace = TRUE)
    refit_spec <- spec
    refit_spec$data <- spec$data[sampled, , drop = FALSE]
    if (name == "clpn") refit_spec$data$id <- seq_len(nrow(refit_spec$data))
    set.seed(918)
    historical_refit <- do.call(fun, refit_spec)
    path <- file.path(output, "objects", paste0(name, ".rds"))
    saveRDS(fit, path, compress = "xz", version = 3)
    saveRDS(list(data = refit_spec$data, graph = historical_refit$graph,
                 networks = historical_refit$networks, meta = historical_refit$meta),
            file.path(output, "objects", paste0(name, "-oracle.rds")), compress = "xz", version = 3)
    manifest$cases[[name]] <- list(file = basename(path), bytes = file.info(path)$size,
      model = fit$model, rows = nrow(fit$data), settings = fit$meta[c("gamma", "cor_method", "missing", "AND", "standardize", "standardize_data")])
    message("Created ", label, "/", name)
  }
  saveRDS(manifest, file.path(output, "manifest.rds"), version = 3)
  jsonlite::write_json(manifest, file.path(output, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
}

consume_objects <- function(source, output) {
  pkgload::load_all(source, quiet = TRUE)
  manifest <- readRDS(file.path(output, "manifest.rds"))
  grDevices::pdf(file.path(output, "current-plots.pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  check <- function(value, label) if (!isTRUE(value)) stop(label, call. = FALSE)
  eq <- function(a, b, label, tolerance = 1e-8) check(isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE)), label)
  results <- list(label = manifest$label, created_with = manifest$versions, read_with = versions(), cases = list())
  for (name in names(manifest$cases)) {
    warnings <- character()
    result <- tryCatch(withCallingHandlers({
      path <- file.path(output, "objects", manifest$cases[[name]]$file)
      fit <- readRDS(path)
      oracle <- readRDS(file.path(output, "objects", paste0(name, "-oracle.rds")))
      check(inherits(fit, "quicknet_fit"), "class changed")
      check(length(capture.output(print(fit))) > 0, "print failed")
      check(is.data.frame(summary(fit)), "summary failed")
      plot <- plot(fit, DoNotPlot = TRUE)
      check(inherits(plot, "qgraph"), "plot failed")
      report <- quicknet_report(fit)
      check(is.list(report), "report failed")
      if (name == "clpn") {
        meta <- fit$meta
        refit <- quickNet:::quicknet_refit_with_backend_args(PanelNet, fit,
          data = oracle$data, nodes = meta$nodes, waves = meta$waves, id = meta$id,
          prefix = meta$prefix, standardize = meta$standardize,
          standardize_data = meta$standardize_data %||% FALSE,
          alpha = meta$alpha, lambda_rule = meta$lambda_rule, nfolds = meta$nfolds, seed = meta$seed)
        eq(refit$fit$design$predictors, quickNet:::quicknet_clpn_design(oracle$data, meta$nodes, meta$waves,
          meta$id, meta$prefix, standardize = TRUE)$predictors, "panel preprocessing differs")
      } else {
        settings <- quickNet:::quicknet_cross_refit_args(fit)
        value <- function(parameter) report$estimation$value[match(parameter, report$estimation$parameter)]
        if (fit$model == "EBICglasso") {
          eq(value("cor_method"), settings$cor_method, "report misstates effective correlation")
          eq(value("missing"), settings$missing, "report misstates effective missing-data rule")
        }
        set.seed(918)
        refit <- quickNet:::quicknet_refit_like(oracle$data, fit)
        eq(quickNet:::quicknet_nct_fit_settings(fit), settings, "NCT settings differ")
        set.seed(918)
        eq(quickNet:::quicknet_nct_refit(oracle$data, settings), oracle$graph, "NCT historical refit differs")
        set.seed(716)
        comparison <- NetCompare(fit, fit, it = 2, progressbar = FALSE)
        eq(comparison$nw1, fit$graph, "NetCompare changed original graph")
        set.seed(716)
        stability <- Stability(fit, nboot = 3, case.drop = .1)
        eq(stability$fit$graph, fit$graph, "Stability changed original graph")
      }
      eq(refit$graph, oracle$graph, "historical estimator controls were not preserved")
      check(all(vapply(refit$networks, function(x) all(is.finite(x)), logical(1))), "nonfinite network")
      roundtrip <- tempfile(fileext = ".rds")
      saveRDS(fit, roundtrip, version = 3)
      eq(readRDS(roundtrip)$graph, fit$graph, "second serialization changed graph", 0)
      unlink(roundtrip)
      list(status = "passed", refit_max_abs_difference = max(abs(refit$graph - oracle$graph)))
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) list(status = "failed", error = conditionMessage(e)))
    result$warnings <- unique(warnings)
    results$cases[[name]] <- result
    message("Validated ", manifest$label, "/", name, ": ", result$status,
      if (!is.null(result$error)) paste0(" - ", result$error) else "")
  }
  saveRDS(results, file.path(output, "validation.rds"), version = 3)
  jsonlite::write_json(results, file.path(output, "validation.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (any(vapply(results$cases, function(x) x$status != "passed", logical(1)))) quit(status = 1L)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
if (length(args) && args[[1L]] == "--create") {
  create_objects(args[[2L]], args[[3L]], args[[4L]])
} else if (length(args) && args[[1L]] == "--consume") {
  consume_objects(args[[2L]], args[[3L]])
} else {
  output <- if (length(args)) args[[1L]] else file.path(root, "..", "output", "audit", "legacy-objects")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output)
  commits <- c("29a414b", "72666a3", "current")
  run <- function(arguments, log) {
    status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script), shQuote(arguments)), stdout = log, stderr = log)
    if (status != 0) stop("Subprocess failed; inspect ", log, call. = FALSE)
  }
  for (commit in commits) {
    case_dir <- file.path(output, commit)
    dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
    if (commit == "current") source <- root else {
      source <- file.path(case_dir, "source")
      dir.create(source, showWarnings = FALSE)
      archive <- file.path(case_dir, "source.tar")
      full_commit <- system2("git", c("-C", shQuote(root), "rev-parse", commit), stdout = TRUE)
      status <- system2("git", c("-C", shQuote(root), "archive", "--format=tar", paste0("--output=", shQuote(archive)), full_commit))
      if (status != 0) stop("git archive failed", call. = FALSE)
      utils::untar(archive, exdir = source)
      writeLines(full_commit, file.path(case_dir, "source-commit.txt"))
    }
    run(c("--create", source, case_dir, commit), file.path(case_dir, "create.log"))
    run(c("--consume", root, case_dir), file.path(case_dir, "consume.log"))
    message("Completed ", commit)
  }
}
