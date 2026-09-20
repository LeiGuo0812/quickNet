#!/usr/bin/env Rscript
# Run from package root. Sections: parity, row, cluster, cluster_ols, nct, all; optional output directory.
# Independent outer datasets are the units for coverage Monte Carlo uncertainty.
args <- commandArgs(trailingOnly = TRUE)
section <- if (length(args)) args[[1]] else "all"
out <- if (length(args) > 1) args[[2]] else "../output/audit/resampling"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
writeLines(c(paste0("section=", section), "master_seed=20260923", capture.output(sessionInfo())),
           file.path(out, paste0("provenance-", section, ".txt")))
cor_graph <- function(data) { g <- cor(data); diag(g) <- 0; g }
interval_summary <- function(raw) {
  groups <- split(raw, interaction(raw$scenario, raw$edge, drop = TRUE))
  do.call(rbind, lapply(groups, function(x) {
    defined <- is.finite(x$lower) & is.finite(x$upper)
    hits <- sum(defined & x$lower <= x$truth & x$upper >= x$truth)
    n <- sum(defined)
    ci <- if (n) as.numeric(binom.test(hits, n)$conf.int) else c(NA, NA)
    data.frame(scenario = x$scenario[1], edge = x$edge[1], truth = x$truth[1], outer_requested = nrow(x),
      outer_valid = n, coverage = if (n) hits / n else NA, mcse = if (n) sqrt(hits/n*(1-hits/n)/n) else NA,
      ci_lower = ci[1], ci_upper = ci[2], bootstrap_requested = sum(x$boot_requested),
      bootstrap_failed = sum(x$boot_failed), original_fit_failed = sum(x$original_failed))
  }))
}

if (section %in% c("all", "parity")) {
  set.seed(92441)
  data <- as.data.frame(matrix(rnorm(400), 100, 4)); names(data) <- letters[1:4]
  data$b <- data$b + .5 * data$a; data$d <- data$d + .3 * data$b
  original <- quickNet(data, model = "correlation", cor_method = "pearson")
  # Actual native bootnet draws; replay the stored data, without copying its estimator.
  native <- quiet(bootnet::bootnet(data, nBoots = 39, default = "cor", corMethod = "cor",
    nCores = 1, statistics = "edge", memorysaver = FALSE, verbose = FALSE))
  replay <- lapply(native$boots, function(b) quicknet_refit_like(b$data, original)$graph)
  delta <- vapply(seq_along(replay), function(i) max(abs(replay[[i]] - native$boots[[i]]$graph)), numeric(1))
  arr <- array(unlist(replay), c(4, 4, length(replay)))
  summary <- quicknet_matrix_bootstrap_summary(original$graph, arr)
  interval_delta <- vapply(seq_len(nrow(summary)), function(i) {
    a <- summary$node_i[i]; b <- summary$node_j[i]
    vals <- vapply(native$boots, function(boot) boot$graph[a, b], numeric(1))
    max(abs(c(summary$ci_lower[i], summary$ci_upper[i]) - quantile(vals, c(.025, .975))))
  }, numeric(1))
  saveRDS(list(native = native, replay = replay, summary = summary), file.path(out, "bootnet-native-draws.rds"))
  # Its CS statistic has a different estimand and sampling grid from the custom table.
  native_case <- quiet(bootnet::bootnet(data, nBoots = 99, default = "cor", corMethod = "cor", type = "case",
    caseMin = .1, caseMax = .5, caseN = 3, statistics = "strength", verbose = FALSE))
  native_cs <- quiet(bootnet::corStability(native_case, statistics = "strength", verbose = FALSE))
  original_strength <- subset(native_case$sampleTable, type == "strength")
  case_rows <- lapply(split(subset(native_case$bootTable, type == "strength"), native_case$bootTable$name[native_case$bootTable$type == "strength"]), function(x) {
    y <- original_strength$value[match(x$node1, original_strength$node1)]
    data.frame(drop = 1 - x$nPerson[1] / native_case$sample$nPerson, correlation = cor(x$value, y))
  })
  cs_probabilities <- aggregate(correlation ~ drop, do.call(rbind, case_rows), function(x) mean(x > .7))
  cs_reference <- max(c(0, cs_probabilities$drop[cs_probabilities$correlation > .95]))
  saveRDS(list(native = native_case, cs = native_cs, reference = cs_reference), file.path(out, "bootnet-native-cs.rds"))
  # Official NCT source was frozen in the prior paired audit. Fetch if absent.
  source_dir <- "../output/audit/inference"
  upstream <- new.env(parent = asNamespace("quickNet"))
  commit <- "f05b102f2fa5c64da2f41e39052acbce18cff633"
  for (filename in c("NCT.R", "estimators.R")) {
    local <- file.path(source_dir, paste0("upstream-NCT-", filename))
    if (!file.exists(local)) {
      dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
      download.file(paste0("https://raw.githubusercontent.com/cvborkulo/NetworkComparisonTest/", commit, "/R/", filename), local, quiet = TRUE)
    }
    sys.source(local, envir = upstream)
  }
  call_native <- function(fun, args) {
    env <- list2env(c(list(fun = fun), args), parent = parent.frame())
    expr <- as.call(c(list(as.name("fun")), setNames(lapply(names(args), as.name), names(args))))
    eval(expr, env)
  }
  source_parity <- lapply(c("none", "holm", "BH"), function(adjust) {
    a <- list(data1 = data[1:50, ], data2 = data[51:100, ], estimator = cor_graph,
      it = 39, test.edges = TRUE, test.centrality = TRUE,
      centrality = c("strength", "expectedInfluence"), p.adjust.methods = adjust,
      progressbar = FALSE, verbose = FALSE)
    set.seed(624); native <- quiet(call_native(upstream$NCT, a))
    set.seed(624); actual <- quiet(do.call(NCT_gl, a))
    fields <- c("nwinv.perm", "nwinv.pval", "glstrinv.perm", "glstrinv.pval", "diffcen.perm", "diffcen.pval")
    difference <- max(vapply(fields, function(field) max(abs(actual[[field]] - native[[field]])), numeric(1)),
                      abs(actual$einv.pvals$`p-value` - native$einv.pvals$`p-value`))
    saveRDS(list(native = native, actual = actual, commit = commit), file.path(out, paste0("nct-", adjust, ".rds")))
    data.frame(comparison = paste0("NCT_", adjust), max_abs_difference = difference)
  })
  default_parity <- lapply(c(FALSE, TRUE), function(binary) {
    input <- if (binary) (as.matrix(data) > .8) * 1 else data
    a <- list(data1 = input[1:40, ], data2 = input[41:100, ], it = 19,
      binary.data = binary, gamma = if (binary) .25 else .5,
      test.edges = TRUE, test.centrality = TRUE, centrality = "strength",
      p.adjust.methods = "holm", progressbar = FALSE, verbose = FALSE)
    set.seed(724); native <- quiet(call_native(upstream$NCT, a))
    set.seed(724); actual <- quiet(do.call(NCT_gl, a))
    fields <- c("nwinv.perm", "nwinv.pval", "glstrinv.perm", "glstrinv.pval", "diffcen.perm", "diffcen.pval")
    difference <- max(vapply(fields, function(field) max(abs(actual[[field]] - native[[field]])), numeric(1)),
                      abs(actual$einv.pvals$`p-value` - native$einv.pvals$`p-value`))
    name <- if (binary) "NCT_default_Ising" else "NCT_default_GGM"
    saveRDS(list(native = native, actual = actual, commit = commit), file.path(out, paste0(name, ".rds")))
    data.frame(comparison = name, max_abs_difference = difference)
  })
  source_parity <- c(source_parity, default_parity)
  ape_delta <- vapply(c("two.sided", "greater", "less"), function(alternative) {
    a <- cor(data[1:50, ]); b <- cor(data[51:100, ])
    set.seed(955); actual <- netCor(a, b, 99, alternative = alternative)
    set.seed(955); expected <- ape::mantel.test(a, b, 99, alternative = alternative)
    max(abs(c(actual$z.stat - expected$z.stat, actual$p - expected$p)))
  }, numeric(1))
  parity <- rbind(data.frame(comparison = c("bootnet_draws", "bootstrap_percentiles", "bootnet_CS", paste0("ape_", names(ape_delta))),
                             max_abs_difference = c(max(delta), max(interval_delta), abs(native_cs - cs_reference), ape_delta)),
                  do.call(rbind, source_parity))
  write.csv(parity, file.path(out, "source-parity.csv"), row.names = FALSE)
  print(parity); stopifnot(max(parity$max_abs_difference) < 1e-10)
  # Fixed rare-binary sample: failure rate, not a claim about population coverage.
  set.seed(141); sparse <- as.data.frame(matrix(0, 30, 3)); names(sparse) <- letters[1:3]
  for (j in 1:3) sparse[sample(30, 2), j] <- 1
  fit <- quiet(quickNet(sparse, model = "ising"))
  bootstrap <- quiet(quicknet_bootstrap_edge_stability(fit, 199, seed = 34))
  info <- attr(bootstrap, "resampling")
  error_ci <- as.numeric(binom.test(info$failed, info$requested)$conf.int)
  write.csv(data.frame(requested = info$requested, failed = info$failed, success = info$succeeded,
    failure_rate = info$failed/info$requested, mcse = sqrt(info$failed/info$requested*(1-info$failed/info$requested)/info$requested),
    ci_lower = error_ci[1], ci_upper = error_ci[2]), file.path(out, "sparse-binary-failure-rate.csv"), row.names = FALSE)
  write.csv(info$failure_details, file.path(out, "sparse-binary-failure-details.csv"), row.names = FALSE)
  saveRDS(list(data = sparse, result = bootstrap), file.path(out, "sparse-binary-raw.rds"))
  # Record the exhaustive independent NCT test (20 group allocations, eight corrections).
  test_result <- testthat::test_file("tests/testthat/test-resampling-validation.R", reporter = "summary", stop_on_failure = TRUE)
  saveRDS(test_result, file.path(out, "formula-and-failure-tests.rds"))
}

if (section %in% c("all", "row")) {
  rows <- list(); raw <- list(); index <- 0L
  scenarios <- c("correlation_zero", "correlation_moderate", "EBICglasso_weak")
  for (scenario in scenarios) {
    if (scenario == "EBICglasso_weak") {
      precision <- diag(3); precision[1, 2] <- precision[2, 1] <- -.15
      precision[2, 3] <- precision[3, 2] <- -.15
      covariance <- solve(precision); truth <- c(a_b = .15, a_c = 0)
    } else {
      covariance <- diag(3); covariance[1, 2] <- covariance[2, 1] <- if (scenario == "correlation_zero") 0 else .3
      truth <- c(a_b = covariance[1, 2], a_c = 0)
    }
    model <- if (scenario == "EBICglasso_weak") "EBICglasso" else "correlation"
    for (i in 1:120) {
      set.seed(31000 + match(scenario, scenarios) * 1000 + i)
      data <- as.data.frame(matrix(rnorm(60 * 3), 60) %*% chol(covariance)); names(data) <- letters[1:3]
      fit <- quiet(quickNet(data, model = model, cor_method = "pearson"))
      bootstrap <- quiet(quicknet_bootstrap_edge_stability(fit, 99, seed = 35000 + match(scenario, scenarios) * 1000 + i))
      index <- index + 1L
      raw[[index]] <- list(scenario = scenario, iteration = i, covariance = covariance, data = data,
                           graph = fit$graph, bootstrap = bootstrap, gamma = quicknet_fit_gamma(fit))
      for (edge in names(truth)) {
        pair <- strsplit(edge, "_")[[1]]
        entry <- bootstrap[bootstrap$node_i == pair[1] & bootstrap$node_j == pair[2], ]
        rows[[length(rows) + 1L]] <- data.frame(scenario = scenario, iteration = i, edge = edge, truth = truth[[edge]],
          lower = entry$ci_lower, upper = entry$ci_upper, boot_requested = 99L, boot_failed = entry$failed_bootstraps, original_failed = FALSE)
      }
    }
    cat("completed", scenario, "\n")
  }
  rows <- do.call(rbind, rows)
  write.csv(rows, file.path(out, "row-coverage-replicates.csv"), row.names = FALSE)
  write.csv(interval_summary(rows), file.path(out, "row-coverage-summary.csv"), row.names = FALSE)
  saveRDS(raw, file.path(out, "row-coverage-raw.rds"))
  print(interval_summary(rows))
}

if (section %in% c("all", "cluster")) {
  rows <- list(); raw <- list()
  for (n in c(12L, 80L)) for (i in 1:80) {
    set.seed(51000 + n * 100 + i)
    initial <- matrix(rnorm(n * 2), n)
    # Three waves: temporal dependence within participants, independent innovations.
    # No random intercept; the homogeneous VAR coefficient is the known truth.
    transition <- matrix(c(.3, .25, 0, .3), 2)
    wave2 <- initial %*% t(transition) + matrix(rnorm(n * 2), n)
    wave3 <- wave2 %*% t(transition) + matrix(rnorm(n * 2), n)
    data <- data.frame(id = seq_len(n), a_t1 = initial[, 1], b_t1 = initial[, 2],
      a_t2 = wave2[, 1], b_t2 = wave2[, 2], a_t3 = wave3[, 1], b_t3 = wave3[, 2])
    fit <- quiet(PanelNet(data, nodes = c("a", "b"), waves = 1:3, nfolds = 3,
      standardize = FALSE, standardize_data = FALSE, seed = 81000 + i))
    result <- quiet(LongitudinalStability(fit, 79, seed = 91000 + n * 100 + i, nfolds = 3))
    raw[[length(raw) + 1L]] <- list(n = n, iteration = i, transition = transition, data = data,
                                  original_graph = fit$graph, bootstrap = result)
    for (edge in c("a_b", "b_a")) {
      pair <- strsplit(edge, "_")[[1]]
      entry <- result$default[result$default$from == pair[1] & result$default$to == pair[2], ]
      rows[[length(rows) + 1L]] <- data.frame(scenario = paste0("CLPN_subjects_", n), iteration = i, edge = edge,
        truth = if (edge == "a_b") .25 else 0, lower = entry$ci_lower, upper = entry$ci_upper,
        boot_requested = 79L, boot_failed = entry$failed_bootstraps, original_failed = FALSE)
    }
    if (i %% 20 == 0) cat("completed CLPN", n, "subjects", i, "of 80\n")
  }
  rows <- do.call(rbind, rows)
  write.csv(rows, file.path(out, "cluster-coverage-replicates.csv"), row.names = FALSE)
  write.csv(interval_summary(rows), file.path(out, "cluster-coverage-summary.csv"), row.names = FALSE)
  saveRDS(raw, file.path(out, "cluster-coverage-raw.rds"))
  print(interval_summary(rows))
}

if (section %in% c("all", "nct", "nct_continuous", "nct_binary")) {
  scenarios <- if (section == "nct_continuous") "unequal_GGM" else if (section == "nct_binary") "sparse_Ising" else c("unequal_GGM", "sparse_Ising")
  for (scenario in scenarios) {
    rows <- raw <- list()
    for (i in 1:100) {
      set.seed(151000 + match(scenario, c("unequal_GGM", "sparse_Ising")) * 1000 + i)
      binary <- scenario == "sparse_Ising"
      if (binary) {
        first <- matrix(rbinom(80 * 3, 1, .1), 80)
        second <- matrix(rbinom(120 * 3, 1, .1), 120)
      } else {
        covariance <- matrix(.25, 3, 3); diag(covariance) <- 1
        first <- matrix(rnorm(40 * 3), 40) %*% chol(covariance)
        second <- matrix(rnorm(80 * 3), 80) %*% chol(covariance)
      }
      colnames(first) <- colnames(second) <- letters[1:3]
      calls <- 0L; warnings <- character(); error <- NA_character_
      estimator <- function(data) {
        calls <<- calls + 1L
        if (binary) NCT_estimator_Ising(data, gamma = .25) else NCT_estimator_GGM(data, gamma = .5)
      }
      set.seed(161000 + match(scenario, c("unequal_GGM", "sparse_Ising")) * 1000 + i)
      result <- tryCatch(withCallingHandlers(NCT_gl(first, second, estimator = estimator,
        binary.data = binary, paired = FALSE, it = 99, test.edges = TRUE,
        test.centrality = TRUE, centrality = "strength", p.adjust.methods = "holm",
        progressbar = FALSE, verbose = FALSE), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
        }), error = function(e) { error <<- conditionMessage(e); NULL })
      metrics <- if (is.null(result)) rep(NA_real_, 4) else c(result$glstrinv.pval, result$nwinv.pval,
        if (all(is.finite(result$einv.pvals$`p-value`))) min(result$einv.pvals$`p-value`) else NA_real_,
        if (all(is.finite(result$diffcen.pval))) min(result$diffcen.pval) else NA_real_)
      rows[[i]] <- data.frame(scenario = scenario, iteration = i, metric = c("global_strength", "maximum_edge", "Holm_edge_family", "Holm_strength_family"),
        p_value = metrics, estimator_calls = calls, failed = is.null(result),
        failure_stage = if (!is.null(result)) NA_character_ else if (calls == 0) "input_validation" else if (calls <= 2) "observed_network" else "permutation_network",
        error = error, rejected_binary_draws = if (is.null(result)) NA_integer_ else result$info$permutation$rejected_draws)
      raw[[i]] <- list(first = first, second = second, result = result, error = error,
                       warnings = unique(warnings), gamma = if (binary) .25 else .5)
      if (i %% 20 == 0) cat("completed", scenario, i, "of 100\n")
    }
    rows <- do.call(rbind, rows)
    summary <- do.call(rbind, lapply(split(rows, rows$metric), function(x) {
      valid <- is.finite(x$p_value); n <- sum(valid); k <- sum(x$p_value[valid] <= .05)
      ci <- if (n) as.numeric(binom.test(k, n)$conf.int) else c(NA_real_, NA_real_)
      data.frame(scenario = scenario, metric = x$metric[1], requested = nrow(x), failed_calls = sum(x$failed),
        undefined_metric = sum(!valid & !x$failed), valid = n, rejected = k,
        rejection_rate = if (n) k/n else NA_real_, mcse = if (n) sqrt(k/n*(1-k/n)/n) else NA_real_,
        ci_lower = ci[1], ci_upper = ci[2])
    }))
    write.csv(rows, file.path(out, paste0("nct-", scenario, "-replicates.csv")), row.names = FALSE)
    write.csv(summary, file.path(out, paste0("nct-", scenario, "-summary.csv")), row.names = FALSE)
    saveRDS(raw, file.path(out, paste0("nct-", scenario, "-raw.rds")))
    print(summary)
  }
}

if (section %in% c("all", "cluster_ols")) {
  # Diagnostic oracle with stats::lm.fit on the exact same generated panel data.
  # This does not change PanelNet's estimator or recommend a different default.
  datasets <- readRDS(file.path(out, "cluster-coverage-raw.rds"))
  ols <- function(data) {
    predictors <- rbind(cbind(data$a_t1, data$b_t1), cbind(data$a_t2, data$b_t2))
    outcomes <- rbind(cbind(data$a_t2, data$b_t2), cbind(data$a_t3, data$b_t3))
    graph <- t(stats::lm.fit(cbind(1, predictors), outcomes)$coefficients[-1, , drop = FALSE])
    dimnames(graph) <- list(c("a", "b"), c("a", "b"))
    graph
  }
  rows <- raw <- list()
  for (i in seq_along(datasets)) {
    item <- datasets[[i]]
    fit <- list(data = item$data, meta = list(id = "id"))
    fit$data$.audit_source_id <- item$data$id
    base_seed <- 91000 + item$n * 100 + item$iteration
    # Validate the source RNG use, then preserve the exact draws of the CLPN run.
    if (i %in% c(1L, 81L)) {
      set.seed(base_seed + 1L)
      invisible(sample(rep(1:3, length.out = item$n)))
      expected_rng <- .Random.seed
      invisible(quiet(PanelNet(item$data, nodes = c("a", "b"), waves = 1:3,
        nfolds = 3, standardize = FALSE, standardize_data = FALSE, seed = base_seed + 1L)))
      stopifnot(identical(expected_rng, .Random.seed))
    }
    sampled_indices <- vector("list", 79)
    refit <- function(data, replication) {
      sampled_indices[[replication]] <<- data$.audit_source_id
      graph <- ols(data)
      set.seed(base_seed + replication)
      invisible(sample(rep(1:3, length.out = item$n)))
      list(networks = list(default = graph))
    }
    result <- quicknet_cluster_bootstrap_stability(fit, 79, base_seed,
      refit, list(default = ols(item$data)), c(default = TRUE))
    raw[[i]] <- list(n = item$n, iteration = item$iteration, bootstrap = result, sampled_indices = sampled_indices)
    for (edge in c("a_b", "b_a")) {
      pair <- strsplit(edge, "_")[[1]]
      entry <- result$default[result$default$from == pair[1] & result$default$to == pair[2], ]
      rows[[length(rows) + 1L]] <- data.frame(scenario = paste0("OLS_subjects_", item$n), iteration = item$iteration,
        edge = edge, truth = if (edge == "a_b") .25 else 0, lower = entry$ci_lower, upper = entry$ci_upper,
        boot_requested = 79L, boot_failed = entry$failed_bootstraps, original_failed = FALSE)
    }
  }
  rows <- do.call(rbind, rows)
  write.csv(rows, file.path(out, "cluster-ols-coverage-replicates.csv"), row.names = FALSE)
  write.csv(interval_summary(rows), file.path(out, "cluster-ols-coverage-summary.csv"), row.names = FALSE)
  saveRDS(raw, file.path(out, "cluster-ols-coverage-raw.rds"))
  print(interval_summary(rows))
}
