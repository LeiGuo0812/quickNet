gamma_data <- function() {
  set.seed(20260920)
  x <- as.data.frame(matrix(rnorm(180 * 3), 180, 3) + rnorm(180))
  names(x) <- c("a", "b", "c")
  x
}

gamma_fit <- function(data, model, ...) {
  suppressWarnings(quickNet(data, model = model, pie = FALSE, DoNotPlot = TRUE, ...))
}

test_that("cross-sectional defaults reach the backend and match direct estimation", {
  x <- gamma_data()
  b <- as.data.frame(lapply(x, function(z) as.integer(z > 0)))
  ising <- gamma_fit(b, "ising")
  direct <- IsingFit::IsingFit(b, gamma = 0.25, plot = FALSE, progressbar = FALSE)
  expect_equal(ising$meta$gamma, 0.25)
  expect_equal(ising$fit$gamma, 0.25)
  expect_equal(ising$graph, direct$weiadj)
  expect_equal(gamma_fit(b, "ising", gamma = NULL)$graph, ising$graph)

  ebic <- gamma_fit(x, "EBICglasso")
  expect_equal(ebic$meta$gamma, 0.5)
  expect_equal(ebic$fit$arguments$tuning, 0.5)
  expect_equal(suppressWarnings(EBICglassoNet(x))$graph, ebic$graph)
  mgm <- gamma_fit(x, "mgm", types = rep("g", 3), levels = rep(1, 3))
  expect_null(mgm$meta$gamma)
  expect_equal(mgm$fit$call$lambdaGam, 0.25)
  expect_equal(mgm$meta$lambdaSel, "CV")

  for (model in c("ising", "EBICglasso", "mgm")) {
    fit <- do.call(gamma_fit, c(list(data = if (model == "ising") b else x, model = model, gamma = 0,
                     types = rep("g", 3), levels = rep(1, 3)), if (model == "mgm") list(lambdaSel = "EBIC")))
    expect_equal(quicknet_fit_gamma(fit), 0)
    refit <- suppressWarnings(quicknet_refit_like(fit$data, fit))
    expect_equal(refit$meta$gamma, 0)
    expect_equal(refit$graph, fit$graph)
  }
})

test_that("inactive gamma is absent from metadata and reports, including old fits", {
  x <- gamma_data()
  for (model in c("correlation", "partial", "ordinal")) {
    data <- if (model == "ordinal") as.data.frame(lapply(x, function(z) rank(z) %% 5 + 1)) else x
    fit <- gamma_fit(data, model, ordinal_method = "spearman")
    changed <- gamma_fit(data, model, gamma = 0.9, ordinal_method = "spearman")
    expect_null(fit$meta$gamma)
    expect_null(changed$meta$gamma)
    expect_equal(changed$graph, fit$graph)
    fit$meta$gamma <- 0.5 # Metadata written by earlier package versions.
    expect_false("gamma" %in% quicknet_report(fit)$estimation$parameter)
    expect_null(quicknet_refit_like(data, fit)$meta$gamma)
  }
  fit <- gamma_fit(x, "partial")
  fit$model <- "confirmatory_ising"
  fit$meta$gamma <- 0.5
  expect_null(quicknet_fit_gamma(fit))
})

test_that("invalid gamma is rejected before backend estimation", {
  for (gamma in list(NA_real_, NaN, Inf, -0.1, 1.1, c(0.25, 0.5), "0.25", TRUE, numeric())) {
    expect_error(quicknet_fit_cross_sectional(gamma_data(), gamma = gamma), "gamma must")
    expect_error(NetworkPower(gamma = gamma), "gamma must")
    expect_error(NCT_gl(gamma_data(), gamma_data(), gamma = gamma, it = 1), "gamma must")
  }
  expect_equal(quicknet_resolve_gamma("ising", 1), 1)
})

test_that("Stability retains the actual fitted gamma and recovers backend evidence", {
  b <- as.data.frame(lapply(gamma_data(), function(z) as.integer(z > 0)))
  fit <- gamma_fit(b, "ising", gamma = 0.5)
  fit$meta$gamma <- NULL
  expect_equal(quicknet_fit_gamma(fit), 0.5)
  expect_equal(quicknet_refit_like(b, fit)$meta$gamma, 0.5)
  expect_match(paste(capture.output(print(fit)), collapse = " "), "fit is 0.5")
  report <- quicknet_report(fit)
  expect_equal(report$estimation$value[report$estimation$parameter == "gamma"], "0.5")
  expect_match(report$text, "defaults to gamma = 0.25 for Ising")

  original <- quicknet_fit_cross_sectional
  seen <- numeric()
  local_mocked_bindings(quicknet_fit_cross_sectional = function(..., gamma = NULL) {
    seen <<- c(seen, gamma)
    original(..., gamma = gamma)
  })
  stability <- Stability(fit, nboot = 2, case.drop = 0.25)
  expect_equal(stability$fit$meta$gamma, 0.5)
  expect_gt(length(seen), 0)
  expect_true(all(seen == 0.5))
  expect_error(Stability(fit, gamma = 0.25, nboot = 1), "cannot override")
  raw <- Stability(b, model = "ising", nboot = 1, case.drop = 0.25)
  expect_equal(raw$fit$meta$gamma, 0.25)

  fit$fit$gamma <- NULL
  expect_null(quicknet_fit_gamma(fit))
  expect_match(quicknet_report(fit)$text, "not recorded")
  expect_error(quicknet_refit_like(b, fit), "gamma is unknown")
  expect_error(Stability(fit, nboot = 1), "gamma is unknown")
})

test_that("dynamic mixed models distinguish EBIC gamma from inactive CV parameters", {
  x <- gamma_data()
  for (fun in list(MixedVARNet, TimeVaryingNet)) {
    extra <- if (identical(fun, TimeVaryingNet)) list(estpoints = c(0.3, 0.7), bandwidth = 0.5) else list()
    estimate <- function(...) suppressWarnings(do.call(fun,
      c(list(data = x, types = rep("g", 3), levels = rep(1, 3), lags = 1), extra, list(...))))
    default <- estimate()
    if (identical(fun, MixedVARNet)) expect_null(default$meta$gamma) else expect_equal(default$meta$gamma, 0.25)
    expect_equal(default$fit$call$lambdaGam, 0.25)
    manual <- estimate(lambdaSel = "EBIC", gamma = 0.7)
    expect_equal(manual$meta$gamma, 0.7)
    expect_equal(manual$fit$call$lambdaGam, 0.7)
    cv <- estimate(lambdaSel = "CV", gamma = 0.9, lambdaFolds = 3)
    expect_equal(cv$fit$call$lambdaSel, "CV")
    expect_null(cv$meta$gamma)
    expect_false("gamma" %in% quicknet_report(cv)$estimation$parameter)
    cv$meta$gamma <- 0.9
    expect_null(quicknet_fit_gamma(cv))
  }
})

test_that("NetCompare shares defaults and preserves fitted estimation settings", {
  b <- as.data.frame(lapply(gamma_data(), function(z) as.integer(z > 0)))
  compare <- function(a, b, ...) NetCompare(a, b, it = 1, test.edges = FALSE,
    test.centrality = FALSE, progressbar = FALSE, ...)
  fit <- gamma_fit(b, "ising")
  raw <- compare(b, b, binary.data = TRUE)
  expect_equal(raw$info$call$gamma, 0.25)
  expect_equal(raw$nw1, fit$graph)
  expect_equal(raw$info$call$estimatorArgs$gamma, 0.25)
  manual <- gamma_fit(b, "ising", gamma = 0.7)
  manual$meta$gamma <- NULL
  fitted <- compare(manual, manual)
  expect_equal(fitted$info$call$gamma, 0.7)
  expect_equal(fitted$nw1, manual$graph)
  expect_error(compare(fit, manual), "identical estimation settings")
  expect_error(compare(manual, manual, gamma = 0.25), "cannot override")
  expect_error(compare(fit, b), "two quicknet_fit")
  manual$fit$gamma <- NULL
  expect_error(compare(manual, manual), "gamma is unknown")

  corfit <- gamma_fit(gamma_data(), "correlation")
  cor_result <- compare(corfit, corfit)
  expect_null(cor_result$info$call$gamma)
  expect_equal(cor_result$nw1, corfit$graph)
})

test_that("NetCompare reports bootnet settings without inventing custom estimator gamma", {
  x <- gamma_data()
  fit <- suppressWarnings(bootnet::estimateNetwork(x, default = "EBICglasso", tuning = 0.7, verbose = FALSE))
  result <- suppressWarnings(suppressMessages(NetCompare(fit, fit, it = 1,
    test.edges = FALSE, test.centrality = FALSE, progressbar = FALSE)))
  expect_equal(result$info$call$gamma, 0.7)
  expect_error(NetCompare(fit, fit, gamma = 0.5), "original fit or estimatorArgs")
  custom <- function(x, gamma = 5) { out <- cor(x); diag(out) <- 0; out }
  result <- NetCompare(x, x, estimator = custom, estimatorArgs = list(gamma = 5),
    it = 1, test.edges = FALSE, test.centrality = FALSE, progressbar = FALSE)
  expect_null(result$info$call$gamma)
  expect_equal(result$info$call$estimatorArgs$gamma, 5)
})

test_that("power planning records only an active EBIC gamma", {
  for (estimator in c("EBICglasso", "partial", "correlation")) {
    power <- NetworkPower(nodes = 3, sample_sizes = 40, replications = 1, estimator = estimator)
    if (estimator == "EBICglasso") {
      expect_equal(power$settings$gamma, 0.5)
      expect_equal(power$results$gamma, 0.5)
    } else {
      expect_null(power$settings$gamma)
      expect_true(all(is.na(power$results$gamma)))
      expect_false("gamma" %in% quicknet_report(power)$settings$parameter)
      failed <- quicknet_power_empty_metric(40, 1, NULL, estimator)
      expect_equal(nrow(failed), 1L)
      expect_true(is.na(failed$gamma))
    }
  }
})

test_that("graphicalVAR bootstraps preserve explicit gamma", {
  skip_if_not_installed("graphicalVAR")
  seen <- numeric()
  local_mocked_bindings(mlGraphicalVAR = function(data, vars, gamma, ...) {
    seen <<- c(seen, gamma)
    mat <- matrix(0.1, length(vars), length(vars), dimnames = list(vars, vars))
    diag(mat) <- 0
    list(fixedPDC = mat, fixedPCC = mat, betweenNet = mat)
  }, .package = "graphicalVAR")
  x <- gamma_data()
  x$id <- rep(1:10, each = 18)
  x$day <- 1
  x$beep <- rep(1:18, 10)
  default <- LongitudinalNet(x, vars = c("a", "b", "c"))
  expect_equal(default$meta$gamma, 0.5)
  fit <- LongitudinalNet(x, vars = c("a", "b", "c"), gamma = 0.7)
  expect_equal(fit$meta$gamma, 0.7)
  LongitudinalStability(fit, nboot = 1)
  expect_equal(seen, c(0.5, 0.7, 0.7))
})

test_that("NIRA provenance and text use the actual fitted gamma", {
  b <- as.data.frame(lapply(gamma_data(), function(z) as.integer(z > 0)))
  fit <- gamma_fit(b, "ising", gamma = 0.6)
  fit$meta$gamma <- 0.25 # Prefer the actual backend setting over stale metadata.
  result <- suppressWarnings(NIRA(fit, n_samples = 10, run_moderation = FALSE,
    run_permutation = FALSE, run_stability = FALSE, engine = "native", engine_iterations = 2))
  expect_equal(result$provenance$estimation_gamma, 0.6)
  expect_match(paste(capture.output(print(result)), collapse = " "), "fit is 0.6")
  expect_match(quicknet_report(result)$text, "defaults to gamma = 0.25 for Ising")
  expect_match(quicknet_report(result)$text, "fit is 0.6")
})
