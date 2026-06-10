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
  expect_equal(fit$network_summary$possible_edges, 6)

  stability <- LongitudinalStability(fit, nboot = 1, nfolds = 5)
  expect_true("default" %in% names(stability))
  expect_true(nrow(stability$default) > 0)
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

  stability <- suppressWarnings(LongitudinalStability(fit, nboot = 1))
  expect_true(all(c("temporal", "contemporaneous", "between") %in% names(stability)))
  expect_true(nrow(stability$temporal) > 0)
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
})
