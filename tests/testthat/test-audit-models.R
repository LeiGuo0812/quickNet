test_that("MGM listwise validation uses the same rows as estimation", {
  dat <- data.frame(x = c(1, 2, 3, 4, NA), y = c(5, 3, 2, 4, 1))
  expect_true(check_input(dat, model = "mgm", types = c("g", "g"),
                         levels = c(1, 1), quiet = TRUE)$ok)
  expect_false(check_input(dat, model = "mixedVAR", types = c("g", "g"),
                          levels = c(1, 1), quiet = TRUE)$ok)
  invalid <- data.frame(x = c(1, 1, 1, 2), y = c(1, 2, 3, NA))
  expect_false(check_input(invalid, model = "correlation", quiet = TRUE)$ok)
  names(invalid) <- c("x_t1", "x_t2")
  expect_false(check_input(invalid, model = "clpn", nodes = "x", waves = 1:2,
                          quiet = TRUE)$ok)
})

test_that("input checks reject ambiguous names and invalid temporal inputs", {
  dat <- data.frame(x = 1:6, y = c(2, 5, 3, 6, 1, 4))
  expect_false(check_input(dat, model = "confirmatory_ggm", vars = c("x", "x"),
                          quiet = TRUE)$ok)
  dat$x[1] <- Inf
  expect_false(check_input(dat, model = "correlation", quiet = TRUE)$ok)
  dat$x[1] <- 1
  expect_false(check_input(dat, model = "time_varying_mvar", types = c("g", "g"),
                          levels = c(1, 1), bandwidth = 0, quiet = TRUE)$ok)
  expect_false(check_input(dat, model = "mixedVAR", types = c("g", "g"),
                          levels = c(1, 1), lags = 6, quiet = TRUE)$ok)
  dat$id <- 1
  dat$day <- 1
  dat$beep <- c(1, 1, 2, 3, 4, 5)
  expect_false(check_input(dat, model = "graphicalVAR", vars = c("x", "y"),
                          quiet = TRUE)$ok)
})

test_that("network templates preserve specified constraints or fail explicitly", {
  asymmetric <- matrix(c(0, 1, 0, 0), 2)
  expect_error(quicknet_confirmatory_template(asymmetric, c("x", "y")), "symmetric")
  bad_names <- matrix(1, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(quicknet_confirmatory_template(bad_names, c("x", "y")), "names")
  named <- matrix(1, 2, 2, dimnames = list(c("y", "x"), c("y", "x")))
  expect_equal(rownames(quicknet_confirmatory_template(named, c("x", "y"))), c("x", "y"))
})

test_that("PanelSEMNet residual_cov FALSE fixes endogenous residual covariances", {
  skip_if_not_installed("lavaan")
  set.seed(913)
  dat <- as.data.frame(matrix(rnorm(900), 150, 6))
  names(dat) <- paste0(rep(c("x", "y"), 3), "_t", rep(1:3, each = 2))
  fit <- suppressWarnings(PanelSEMNet(dat, c("x", "y"), 1:3, residual_cov = FALSE))
  pars <- lavaan::parameterTable(fit$fit$model)
  endogenous <- paste0(rep(c("x", "y"), 2), "_t", rep(2:3, each = 2))
  unexpected <- pars$op == "~~" & pars$lhs != pars$rhs &
    pars$lhs %in% endogenous & pars$rhs %in% endogenous & pars$free > 0
  expect_false(any(unexpected))
  fit <- suppressWarnings(PanelSEMNet(dat, c("x", "y"), 1:3, residual_cov = TRUE))
  pars <- lavaan::parameterTable(fit$fit$model)
  expect_true(any(pars$op == "~~" & pars$lhs == "x_t3" & pars$rhs == "y_t3" & pars$free > 0))
})

test_that("RI-CLPM extracts finite innovation layers for each parameterization", {
  skip_if_not_installed("psychonetrics")
  set.seed(914)
  dat <- as.data.frame(matrix(rnorm(1800), 300, 6))
  names(dat) <- paste0(rep(c("x", "y"), 3), "_t", rep(1:3, each = 2))
  for (type in c("cov", "chol", "prec", "ggm")) {
    fit <- suppressWarnings(PanelNet(dat, c("x", "y"), 1:3, model = "ri_clpm", ri_type = type))
    expect_true(all(is.finite(fit$networks$contemporaneous)), info = type)
    expect_true(all(is.finite(fit$networks$random_intercept)), info = type)
    expect_true("in_strength" %in% names(fit$nodes))
    cross_nodes <- fit$nodes[fit$nodes$network == "cross_lagged", ]
    expect_true(all(is.finite(cross_nodes$in_strength)), info = type)
  }
})

test_that("unnamed meta-analysis matrices retain node dimensions", {
  cors <- list(diag(3), diag(3))
  infer <- function(cors) quicknet_meta_infer_vars(NULL, cors, NULL, NULL,
                                                 NULL, "id", "day", "beep", "meta_cor")
  expect_equal(infer(cors), paste0("V", 1:3))
  expect_false(check_input(model = "meta_cor", cors = cors, nobs = c(-1, 10), quiet = TRUE)$ok)
  skip_if_not_installed("psychonetrics")
  set.seed(915)
  cors <- lapply(c(80, 100, 120), function(n) stats::cor(matrix(rnorm(n * 3), n, 3)))
  fit <- suppressWarnings(MetaNet(cors = cors, nobs = c(80, 100, 120), model = "meta_cor"))
  expect_equal(dim(fit$graph), c(3L, 3L))
  expect_equal(rownames(fit$graph), paste0("V", 1:3))
})

test_that("mlVAR does not report an unestimated between-person layer as a network", {
  skip_if_not_installed("mlVAR")
  set.seed(102)
  ids <- rep(1:8, each = 12)
  time <- rep(1:12, times = 8)
  dat <- data.frame(id = ids, day = ceiling(time / 4), beep = ((time - 1) %% 4) + 1,
                    x1 = rnorm(96), x2 = rnorm(96), x3 = rnorm(96))
  captured <- character()
  fit <- withCallingHandlers(LongitudinalNet(dat, vars = c("x1", "x2", "x3"),
                                             model = "mlVAR"), warning = function(w) {
    captured <<- c(captured, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("between-person network.*omitted", captured)))
  expect_false("between" %in% names(fit$networks))
  expect_true(all(vapply(fit$networks, function(x) all(is.finite(x)), logical(1))))
  expect_error(quicknet_mlvar_get_net(list(), "temporal", c("x", "y")),
               "Could not extract the mlVAR temporal network")
})

test_that("CLPN predictive R squared compares mean squared errors on the same scale", {
  skip_if_not_installed("glmnet")
  set.seed(916)
  x <- matrix(rnorm(90), 30, 3, dimnames = list(NULL, letters[1:3]))
  y <- x * 0.3 + matrix(rnorm(90), 30, 3)
  fit <- quicknet_clpn_glmnet(x, y, nfolds = 3)
  for (node in colnames(y)) {
    cv <- fit$fits[[node]]
    index <- which.min(abs(cv$lambda - cv$lambda.1se))
    expected <- 1 - cv$cvm[index] / mean((y[, node] - mean(y[, node]))^2)
    expect_equal(fit$predictability$cv_r_squared[fit$predictability$node == node], expected)
  }
})

test_that("longitudinal bootstrap refits retain user-specified backend arguments", {
  mat <- matrix(c(0.2, 0.1, -0.1, 0.3), 2,
                dimnames = list(c("x", "y"), c("x", "y")))
  fit <- quicknet_fit("panel_var", data = data.frame(id = 1:3),
                     networks = list(default = mat, temporal = mat),
                     meta = list(id = "id", nodes = c("x", "y"), waves = 1:3,
                                 prefix = "_t", standardize = TRUE,
                                 backend_args = list(within_latent = "chol", beta = diag(2))))
  received <- NULL
  testthat::local_mocked_bindings(PanelNet = function(...) {
    received <<- list(...)
    fit
  })
  result <- quicknet_psychonetrics_panel_bootstrap_stability(fit, nboot = 1, seed = 917)
  expect_equal(received$within_latent, "chol")
  expect_equal(received$beta, diag(2))
  expect_true(all(result$temporal$valid_bootstraps == 1))
})
