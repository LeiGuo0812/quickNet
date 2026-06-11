test_that("NetworkPower returns a quicknet_power object", {
  set.seed(11)
  power <- NetworkPower(
    nodes = 4,
    density = 0.40,
    sample_sizes = c(30, 40),
    replications = 2,
    target_value = 0.20,
    target_probability = 0.50,
    seed = 11
  )

  expect_s3_class(power, "quicknet_power")
  expect_true(all(c("results", "summary", "recommendation", "report") %in% names(power)))
  expect_true(all(c("sample_size", "achieved_probability", "mean_sensitivity") %in% names(power$summary)))
  expect_s3_class(plot(power), "ggplot")
  expect_s3_class(plot(power, type = "metric"), "ggplot")

  report <- quicknet_report(power)
  expect_s3_class(report, "quicknet_report")
  expect_true(all(c("settings", "summary", "recommendation", "text") %in% names(report)))
})

test_that("ConfirmatoryNet returns a quicknet_fit object", {
  skip_if_not_installed("psychonetrics")
  set.seed(12)
  dat <- as.data.frame(matrix(rnorm(100 * 4), ncol = 4))
  names(dat) <- paste0("x", 1:4)
  omega <- matrix(1, 4, 4, dimnames = list(names(dat), names(dat)))
  diag(omega) <- 0

  fit <- suppressWarnings(ConfirmatoryNet(dat, omega = omega))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "confirmatory_ggm")
  expect_true("fit_indices" %in% names(fit$fit))
  expect_true(all(c("default") %in% names(fit$networks)))
  expect_s3_class(quicknet_report(fit), "quicknet_report")
})

test_that("LatentNet returns latent and residual network layers", {
  skip_if_not_installed("lavaan")
  set.seed(13)
  latent_1 <- rnorm(160)
  latent_2 <- rnorm(160)
  dat <- data.frame(
    x1 = latent_1 + rnorm(160, sd = 0.4),
    x2 = latent_1 + rnorm(160, sd = 0.4),
    x3 = latent_2 + rnorm(160, sd = 0.4),
    x4 = latent_2 + rnorm(160, sd = 0.4)
  )

  fit <- suppressWarnings(LatentNet(dat, "F1 =~ x1 + x2\nF2 =~ x3 + x4"))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "latent_network")
  expect_true(all(c("latent", "residual") %in% names(fit$networks)))
  expect_true(all(c("fit_indices", "loadings") %in% names(fit$fit)))
  expect_true("latent_variables" %in% names(quicknet_report(fit)$sample))
})

test_that("PanelSEMNet returns a directed quicknet_fit object", {
  skip_if_not_installed("lavaan")
  set.seed(14)
  n <- 80
  dat <- data.frame(id = seq_len(n))
  for (wave in 1:3) {
    dat[[paste0("x1_t", wave)]] <- rnorm(n)
    dat[[paste0("x2_t", wave)]] <- rnorm(n)
  }

  fit <- suppressWarnings(PanelSEMNet(dat, nodes = c("x1", "x2"), waves = 1:3))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "panel_sem")
  expect_true(fit$meta$directed)
  expect_true(all(c("default", "cross_lagged") %in% names(fit$networks)))
  expect_true(any(fit$edges$edge_type == "autoregressive"))
  expect_true("residual_cov" %in% quicknet_report(fit)$estimation$parameter)
})

test_that("MixedVARNet and TimeVaryingNet return quicknet_fit objects", {
  skip_if_not_installed("mgm")
  set.seed(15)
  dat <- data.frame(
    mood = rnorm(90),
    stress = rnorm(90),
    arousal = sample(1:2, 90, replace = TRUE)
  )
  types <- c("g", "g", "c")
  levels <- c(1, 1, 2)

  mvar <- suppressWarnings(MixedVARNet(dat, types = types, levels = levels, signInfo = FALSE))
  expect_s3_class(mvar, "quicknet_fit")
  expect_equal(mvar$model, "mixedVAR")
  expect_true(mvar$meta$directed)
  expect_true("temporal" %in% names(mvar$networks))

  tvmvar <- suppressWarnings(TimeVaryingNet(
    dat,
    types = types,
    levels = levels,
    estpoints = c(0.30, 0.70),
    bandwidth = 0.30
  ))
  expect_s3_class(tvmvar, "quicknet_fit")
  expect_equal(tvmvar$model, "time_varying_mvar")
  expect_true(all(c("estpoint_1", "estpoint_2") %in% names(tvmvar$networks)))
})
