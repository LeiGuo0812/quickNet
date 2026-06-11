test_that("input_requirements returns model-specific input guidance", {
  all_specs <- input_requirements()
  expect_true(all(c("model", "data_shape", "required_arguments", "variable_coding") %in% names(all_specs)))
  expect_true("ising" %in% all_specs$model)

  ising_spec <- input_requirements("ising")
  expect_equal(nrow(ising_spec), 1)
  expect_match(ising_spec$variable_coding, "0/1")

  panel_spec <- input_requirements("PanelNet")
  expect_equal(panel_spec$model, "clpn")
})

test_that("model_registry tracks all input-supported models", {
  registry <- model_registry()
  requirements <- input_requirements()

  expect_true(all(c("model", "family", "backend", "analysis_type", "reportable_parameters", "limitations") %in% names(registry)))
  expect_equal(sort(registry$model), sort(requirements$model))
  expect_equal(anyDuplicated(registry$model), 0L)
  expect_equal(model_registry("ConfirmatoryNet")$model, "confirmatory_ggm")
  expect_match(model_registry("confirmatory_ggm")$reportable_parameters, "parameters")
})

test_that("check_input diagnoses cross-sectional inputs", {
  continuous <- check_input(mtcars[, 1:5], model = "EBICglasso", quiet = TRUE)
  expect_s3_class(continuous, "quicknet_input_check")
  expect_true(continuous$ok)

  invalid_ising <- check_input(mtcars[, 1:3], model = "ising", quiet = TRUE)
  expect_false(invalid_ising$ok)
  expect_match(paste(invalid_ising$errors, collapse = " "), "0/1")

  binary_data <- data.frame(
    b1 = c(0, 1, 0, 1, 0, 1),
    b2 = c(1, 1, 0, 0, 1, 0)
  )
  valid_ising <- check_input(binary_data, model = "ising", quiet = TRUE)
  expect_true(valid_ising$ok)
})

test_that("check_input diagnoses panel and longitudinal inputs", {
  panel_data <- data.frame(
    id = 1:5,
    x1_t1 = rnorm(5),
    x2_t1 = rnorm(5),
    x1_t2 = rnorm(5),
    x2_t2 = rnorm(5)
  )
  panel_check <- check_input(panel_data, model = "clpn", nodes = c("x1", "x2"), waves = 1:2, quiet = TRUE)
  expect_true(panel_check$ok)
  expect_true(check_input(panel_data, model = "panel_gvar", nodes = c("x1", "x2"), waves = 1:2, quiet = TRUE)$ok)
  expect_true(check_input(panel_data, model = "panel_var", nodes = c("x1", "x2"), waves = 1:2, quiet = TRUE)$ok)
  expect_true(check_input(panel_data, model = "ri_clpm", nodes = c("x1", "x2"), waves = 1:2, quiet = TRUE)$ok)

  bad_panel <- check_input(panel_data, model = "clpn", nodes = c("x1", "x3"), waves = 1:2, quiet = TRUE)
  expect_false(bad_panel$ok)
  expect_match(paste(bad_panel$errors, collapse = " "), "Missing column")

  long_data <- data.frame(
    id = rep(1:2, each = 4),
    day = rep(1:2, times = 4),
    beep = rep(1:4, times = 2),
    x1 = rnorm(8),
    x2 = rnorm(8)
  )
  long_check <- check_input(long_data, model = "graphicalVAR", vars = c("x1", "x2"), quiet = TRUE)
  expect_true(long_check$ok)
  expect_true(check_input(long_data, model = "psychonetrics_gvar", vars = c("x1", "x2"), quiet = TRUE)$ok)
})

test_that("check_input diagnoses new model families", {
  dat <- data.frame(x1 = rnorm(20), x2 = rnorm(20), x3 = rnorm(20), x4 = rnorm(20))
  expect_true(check_input(dat, model = "confirmatory_ggm", vars = names(dat), quiet = TRUE)$ok)
  expect_true(check_input(dat, model = "confirmatory_cor", vars = names(dat), quiet = TRUE)$ok)
  expect_true(check_input(dat, model = "confirmatory_precision", vars = names(dat), quiet = TRUE)$ok)
  binary <- data.frame(b1 = c(0, 1, 0, 1), b2 = c(1, 1, 0, 0))
  expect_true(check_input(binary, model = "confirmatory_ising", quiet = TRUE)$ok)
  expect_false(check_input(dat, model = "latent_network", vars = names(dat), quiet = TRUE)$ok)
  expect_true(check_input(dat, model = "latent_network", vars = names(dat), syntax = "F =~ x1 + x2 + x3 + x4", quiet = TRUE)$ok)
  lambda <- matrix(1, 4, 1, dimnames = list(names(dat), "F"))
  expect_false(check_input(dat, model = "lnm", vars = names(dat), quiet = TRUE)$ok)
  expect_true(check_input(dat, model = "lnm", vars = names(dat), lambda = lambda, quiet = TRUE)$ok)
  expect_true(check_input(dat, model = "mixedVAR", types = rep("g", 4), levels = rep(1, 4), quiet = TRUE)$ok)
  expect_false(check_input(dat, model = "mixedVAR", types = rep("g", 3), levels = rep(1, 4), quiet = TRUE)$ok)

  cors <- list(stats::cor(dat), stats::cor(dat + matrix(rnorm(80), ncol = 4)))
  expect_false(check_input(model = "meta_ggm", cors = cors, vars = names(dat), quiet = TRUE)$ok)
  expect_true(check_input(model = "meta_ggm", cors = cors, nobs = c(20, 20), vars = names(dat), quiet = TRUE)$ok)

  power_check <- check_input(model = "power", nodes = 2, quiet = TRUE)
  expect_false(power_check$ok)
})

test_that("model functions use input validation before fitting", {
  expect_error(
    quickNet(mtcars[, 1:3], model = "ising", pie = FALSE, DoNotPlot = TRUE),
    "Input does not match requirements"
  )

  panel_data <- data.frame(id = 1:5, x1_t1 = rnorm(5), x1_t2 = rnorm(5))
  expect_error(
    PanelNet(panel_data, nodes = c("x1", "x2"), waves = 1:2, nfolds = 2),
    "Input does not match requirements"
  )
})
