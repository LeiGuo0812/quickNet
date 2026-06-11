test_that("PanelNet returns a directed quicknet_fit object", {
  skip_if_not_installed("glmnet")
  set.seed(100)
  n <- 80
  dat <- data.frame(id = seq_len(n))
  for (wave in 1:3) {
    dat[[paste0("x1_t", wave)]] <- rnorm(n)
    dat[[paste0("x2_t", wave)]] <- rnorm(n)
    dat[[paste0("x3_t", wave)]] <- rnorm(n)
  }

  fit <- PanelNet(dat, nodes = c("x1", "x2", "x3"), waves = 1:3, nfolds = 5)

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "clpn")
  expect_true(all(c("default", "cross_lagged") %in% names(fit$networks)))
  expect_true(fit$meta$directed)
  expect_true(all(c("in_expected_influence", "out_expected_influence", "cv_r_squared") %in% names(fit$nodes)))
  expect_true(all(c("from", "to", "edge_type", "directed") %in% names(fit$edges)))
  expect_true(any(fit$edges$edge_type == "autoregressive"))
  expect_true(all(c("default", "cross_lagged") %in% fit$network_summary$network))
  expect_true(all(fit$network_summary$possible_edges == 6))
  nonzero_cross_lagged <- sum(abs(fit$networks$cross_lagged) > 1e-10)
  expect_equal(nrow(fit$Edgelist), nonzero_cross_lagged)
  expect_equal(nrow(get_edges_df(fit)), nonzero_cross_lagged)
  expect_true(is.finite(globalCoeff(fit)$globalStrength))

  report <- quicknet_report(fit)
  expect_s3_class(report, "quicknet_report")
  expect_true(all(c("subjects", "waves", "transitions") %in% names(report$sample)))
  expect_true("lambda_rule" %in% report$estimation$parameter)
  expect_equal(report$edges$possible_edges[report$edges$network == "default"], 6)
  expect_equal(report$edges$self_edges[report$edges$network == "default"], 3)
  expect_true("edge_type" %in% names(attr(report$edges, "by_edge_type")))
  expect_true("cv_r_squared" %in% names(report$model_specific))

  stability <- LongitudinalStability(fit, nboot = 1, nfolds = 5)
  expect_true("default" %in% names(stability))
  expect_true(nrow(stability$default) > 0)
})

test_that("PanelNet supports psychonetrics panel models", {
  skip_if_not_installed("psychonetrics")
  set.seed(110)
  n <- 70
  nodes <- c("x1", "x2")
  waves <- 1:3
  x1 <- matrix(rnorm(n * length(waves)), nrow = n)
  x2 <- matrix(rnorm(n * length(waves)), nrow = n)
  for (wave in 2:length(waves)) {
    x1[, wave] <- 0.35 * x1[, wave - 1] + rnorm(n)
    x2[, wave] <- 0.25 * x2[, wave - 1] + 0.10 * x1[, wave - 1] + rnorm(n)
  }
  dat <- data.frame(
    id = seq_len(n),
    x1_t1 = x1[, 1], x2_t1 = x2[, 1],
    x1_t2 = x1[, 2], x2_t2 = x2[, 2],
    x1_t3 = x1[, 3], x2_t3 = x2[, 3]
  )

  panel_gvar <- suppressWarnings(PanelNet(dat, nodes = nodes, waves = waves, model = "panel_gvar"))
  expect_s3_class(panel_gvar, "quicknet_fit")
  expect_equal(panel_gvar$model, "panel_gvar")
  expect_true(all(c("temporal", "within", "between") %in% names(panel_gvar$networks)))
  expect_equal(panel_gvar$meta$backend, "psychonetrics::panelgvar")

  panel_var <- suppressWarnings(PanelNet(dat, nodes = nodes, waves = waves, model = "panel_var"))
  expect_s3_class(panel_var, "quicknet_fit")
  expect_equal(panel_var$model, "panel_var")
  expect_true(all(c("temporal", "within", "between") %in% names(panel_var$networks)))

  ri_clpm <- suppressWarnings(PanelNet(dat, nodes = nodes, waves = waves, model = "ri_clpm"))
  expect_s3_class(ri_clpm, "quicknet_fit")
  expect_equal(ri_clpm$model, "ri_clpm")
  expect_true(all(c("temporal", "cross_lagged", "contemporaneous", "random_intercept") %in% names(ri_clpm$networks)))
  expect_true("ri_type" %in% quicknet_report(ri_clpm)$estimation$parameter)
})

test_that("LongitudinalNet returns graphicalVAR network layers", {
  skip_if_not_installed("graphicalVAR")
  set.seed(101)
  ids <- rep(1:8, each = 12)
  time <- rep(1:12, times = 8)
  dat <- data.frame(
    id = ids,
    day = ceiling(time / 4),
    beep = ((time - 1) %% 4) + 1,
    x1 = rnorm(length(ids)),
    x2 = rnorm(length(ids)),
    x3 = rnorm(length(ids))
  )

  fit <- suppressWarnings(LongitudinalNet(
    dat,
    vars = c("x1", "x2", "x3"),
    id = "id",
    day = "day",
    beep = "beep",
    lambda_min_kappa_fixed = 0.05,
    lambda_min_beta_fixed = 0.05
  ))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "graphicalVAR")
  expect_true(all(c("temporal", "contemporaneous", "between") %in% names(fit$networks)))
  expect_true(all(c("temporal", "contemporaneous", "between") %in% unique(fit$edges$network)))
  expect_true(all(c("default", "temporal", "contemporaneous", "between") %in% summary(fit)$network))

  report <- quicknet_report(fit)
  expect_true(all(c("temporal", "contemporaneous", "between") %in% report$networks$network))
  expect_true("gamma" %in% report$estimation$parameter)

  stability <- suppressWarnings(LongitudinalStability(fit, nboot = 1))
  expect_true(all(c("temporal", "contemporaneous", "between") %in% names(stability)))
  expect_true(nrow(stability$temporal) > 0)
})

test_that("LongitudinalNet supports psychonetrics gvar", {
  skip_if_not_installed("psychonetrics")
  set.seed(111)
  ids <- rep(1:20, each = 5)
  dat <- data.frame(
    id = ids,
    day = 1,
    beep = rep(1:5, times = 20),
    x1 = rnorm(length(ids)),
    x2 = rnorm(length(ids))
  )

  fit <- suppressWarnings(LongitudinalNet(
    dat,
    vars = c("x1", "x2"),
    id = "id",
    day = "day",
    beep = "beep",
    model = "psychonetrics_gvar"
  ))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "psychonetrics_gvar")
  expect_true(all(c("temporal", "contemporaneous") %in% names(fit$networks)))
  expect_equal(fit$meta$backend, "psychonetrics::gvar")
  expect_true(all(c("temporal", "contemporaneous") %in% quicknet_report(fit)$networks$network))
})

test_that("LongitudinalNet supports mlVAR", {
  skip_if_not_installed("mlVAR")
  set.seed(102)
  ids <- rep(1:8, each = 12)
  time <- rep(1:12, times = 8)
  dat <- data.frame(
    id = ids,
    day = ceiling(time / 4),
    beep = ((time - 1) %% 4) + 1,
    x1 = rnorm(length(ids)),
    x2 = rnorm(length(ids)),
    x3 = rnorm(length(ids))
  )

  fit <- suppressWarnings(LongitudinalNet(
    dat,
    vars = c("x1", "x2", "x3"),
    id = "id",
    day = "day",
    beep = "beep",
    model = "mlVAR",
    temporal = "fixed",
    contemporaneous = "fixed",
    nCores = 1
  ))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "mlVAR")
  expect_true(all(c("temporal", "contemporaneous", "between") %in% names(fit$networks)))
  expect_true(all(c("default", "temporal", "contemporaneous", "between") %in% summary(fit)$network))

  report <- quicknet_report(fit)
  expect_true("estimator" %in% report$estimation$parameter)
  expect_true(all(c("temporal", "contemporaneous", "between") %in% report$networks$network))
})
