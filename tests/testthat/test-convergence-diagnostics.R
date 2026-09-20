test_that("actual backend convergence status is reported without an invented tolerance", {
  skip_if_not_installed("lavaan")
  set.seed(20251)
  factor <- rnorm(180)
  dat <- as.data.frame(replicate(4, factor + rnorm(180)))
  names(dat) <- paste0("x", 1:4)
  native <- suppressWarnings(lavaan::cfa("f =~ x1+x2+x3+x4", data = dat,
                                        control = list(iter.max = 1)))
  expect_false(lavaan::lavInspect(native, "converged"))
  diagnostics <- quicknet_backend_diagnostics(native)
  expect_equal(diagnostics$status, "failed")
  expect_warning(fit <- quicknet_fit("cfa", networks = list(default = diag(4)),
                                    fit = list(model = native)), "unsuccessful")
  expect_false(quicknet_fit_is_valid(fit))
  expect_match(quicknet_fit_failure_reason(fit), "non-convergence")
  expect_match(quicknet_report(fit)$text, "unsuccessful")
  expect_output(print(fit), "unsuccessful")
  fit$diagnostics <- NULL
  expect_equal(quicknet_fit_diagnostics(fit), diagnostics)
  successful <- lavaan::cfa("f =~ x1+x2+x3+x4", data = dat)
  expect_equal(quicknet_backend_diagnostics(successful)$status, "ok")
  expect_true(quicknet_backend_diagnostics(successful)$finite_parameters)
})

test_that("psychonetrics optimizer status, not computed alone, defines convergence", {
  skip_if_not_installed("psychonetrics")
  set.seed(206)
  dat <- as.data.frame(matrix(rnorm(800), 200, 4))
  native <- psychonetrics::ggm(dat)
  expect_false(native@computed)
  expect_equal(quicknet_backend_diagnostics(native)$status, "failed")
  successful <- suppressWarnings(quicknet_psychonetrics_run(native))
  expect_equal(quicknet_backend_diagnostics(successful)$converged, successful@optim$convergence == 0)
  # Recreate the documented optimizer return protocol to cover all platforms.
  successful@optim$convergence <- 1L
  successful@optim$message <- "Iteration limit reached"
  expect_true(successful@computed)
  expect_equal(quicknet_backend_diagnostics(successful)$status, "failed")
})

test_that("unavailable backend diagnostics are unknown, not declared successful", {
  fit <- quicknet_fit("correlation", networks = list(default = diag(3)))
  expect_equal(fit$diagnostics$status, "unknown")
  expect_true(quicknet_fit_is_valid(fit))
  expect_null(quicknet_fit_failure_reason(fit))
  expect_equal(quicknet_report(fit)$diagnostics$status, "unknown")
})

test_that("glmnet nonfatal shortened paths retain source-returned solutions", {
  skip_if_not_installed("glmnet")
  set.seed(867)
  x <- matrix(rnorm(1000), 100, 10)
  y <- x[, 1] + rnorm(100)
  shortened <- suppressWarnings(glmnet::glmnet(x, y, maxit = 5))
  expect_lt(shortened$jerr, 0)
  expect_true(length(shortened$lambda) > 0)
  raw <- list(glmnet = list(fits = list(y = shortened)))
  expect_equal(quicknet_backend_diagnostics(raw)$status, "partial")
  expect_warning(fit <- quicknet_fit("clpn", networks = list(default = diag(2)), fit = raw), "partial regularization")
  expect_true(quicknet_fit_is_valid(fit))
  expect_match(quicknet_report(fit)$text, "partial regularization")
})

test_that("reported sample size follows backend FIML exclusion of empty rows", {
  skip_if_not_installed("lavaan")
  set.seed(33)
  f <- rnorm(200)
  d <- as.data.frame(replicate(4, f + rnorm(200)))
  names(d) <- paste0("x", 1:4)
  d[1, ] <- NA
  actual <- suppressWarnings(LatentNet(d, "f =~ x1+x2+x3+x4", missing = "fiml"))
  report <- quicknet_report(actual)
  expect_equal(report$sample$observations, 199)
  expect_equal(report$sample$input_rows, 200)
  expect_true(any(grepl("empty", report$backend_warnings)))
  expect_equal(report$analysis_sample$analyzed_observations,
               lavaan::lavInspect(actual$fit$model, "nobs"), ignore_attr = TRUE)
})
