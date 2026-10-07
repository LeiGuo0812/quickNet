powerly_native_pair <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    truth <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
    truth[cbind(1:4, 2:5)] <- 0.3
    truth <- truth + t(truth)
    controls <- list(
      range_lower = 50, range_upper = 300,
      model_matrix = truth, samples = 5, replications = 5,
      boots = 10, spline_df = 3, iterations = 1, tolerance = 10,
      cores = 1, verbose = FALSE
    )
    set.seed(88021)
    native <- do.call(powerly::powerly, controls)
    wrapped <- do.call(NetworkPower, c(
      list(method = "powerly", seed = 88021), controls
    ))
    cached <<- list(native = native, wrapped = wrapped)
    cached
  }
})

test_that("powerly delegation preserves every native stage under the same seed", {
  skip_if_not_installed("powerly")
  pair <- powerly_native_pair()
  native <- pair$native
  plan <- pair$wrapped

  expect_s3_class(plan, "quicknet_power")
  expect_s3_class(plan$fit, "Method")
  expect_equal(plan$fit$step_1$true_model_parameters,
               native$step_1$true_model_parameters)
  expect_equal(plan$fit$step_1$measures, native$step_1$measures)
  expect_equal(plan$fit$step_1$statistics, native$step_1$statistics)
  expect_equal(plan$fit$step_2$interpolation$fitted,
               native$step_2$interpolation$fitted)
  expect_equal(plan$fit$step_3$ci, native$step_3$ci)
  expect_equal(plan$fit$recommendation, native$recommendation)
  expect_equal(plan$recommendation$backend_recommended_n,
               unname(native$recommendation[["50%"]]))
  expect_equal(plan$recommendation$backend_n_lower,
               unname(native$recommendation[["2.5%"]]))
  expect_equal(plan$recommendation$backend_n_upper,
               unname(native$recommendation[["97.5%"]]))
  expect_identical(plan$settings$backend_version,
                   as.character(utils::packageVersion("powerly")))
  expect_equal(plan$settings$backend_gamma, 0.5)
  expect_equal(plan$settings$backend_data_levels, 5)
  expect_true("native_algorithm" %in% names(plan$settings))
})

test_that("a native recommendation retains its nonconverged algorithm status", {
  skip_if_not_installed("powerly")
  pair <- powerly_native_pair()
  native <- pair$native
  plan <- pair$wrapped

  expect_false(native$converged)
  expect_identical(plan$recommendation$algorithm_converged,
                   native$converged)
  expect_equal(plan$recommendation$algorithm_iterations, native$iteration)
  width <- unname(native$recommendation[["97.5%"]] -
                  native$recommendation[["2.5%"]])
  expect_equal(plan$recommendation$recommendation_interval_width, width)
  expect_gt(width, native$range$tolerance)
  expect_match(plan$report, "converg", ignore.case = TRUE)
})

test_that("native validation uses fresh simulations and retains original output", {
  skip_if_not_installed("powerly")
  pair <- powerly_native_pair()
  set.seed(88022)
  native <- powerly::validate(pair$native, replications = 5,
                             cores = 1, verbose = FALSE)
  validation <- ValidateNetworkPower(pair$wrapped,
    replications = 5, seed = 88022, cores = 1, verbose = FALSE)

  expect_s3_class(validation, "quicknet_power_validation")
  expect_s3_class(validation$fit, "Validation")
  expect_equal(validation$fit$sample, native$sample)
  expect_equal(validation$fit$measures, native$measures)
  expect_equal(validation$fit$statistic, native$statistic)
  expect_equal(validation$fit$percentile_value, native$percentile_value)

  summary <- summary(validation)
  target <- pair$native$step_1$measure_value
  successes <- sum(native$measures >= target)
  repetitions <- length(native$measures)
  probability <- successes / repetitions
  interval <- as.numeric(stats::binom.test(successes, repetitions)$conf.int)
  expect_equal(summary, validation$summary)
  expect_equal(summary$sample_size, as.numeric(native$sample))
  expect_equal(summary$replications, repetitions)
  expect_equal(summary$achieved_replications, successes)
  expect_equal(summary$achieved_probability, probability)
  expect_equal(summary$native_probability, as.numeric(native$statistic))
  expect_equal(summary$probability_mcse,
               sqrt(probability * (1 - probability) / repetitions))
  expect_equal(c(summary$probability_ci_lower, summary$probability_ci_upper),
               interval)
  expect_equal(summary$point_estimate_reaches_target,
               probability >= pair$native$step_1$statistic_value)
  expect_equal(summary$lower_bound_supports_target,
               interval[[1]] >= pair$native$step_1$statistic_value)
  expect_identical(validation$settings$seed, 88022)
  expect_identical(validation$settings$backend_version,
                   as.character(utils::packageVersion("powerly")))
  report <- quicknet_report(validation)
  expect_s3_class(report, "quicknet_report")
  expect_true(all(c("settings", "summary", "text") %in% names(report)))
  expect_equal(report$summary$sample_size, as.numeric(native$sample))
  expect_match(validation$report, "conditional|true network|hypothes", ignore.case = TRUE)
})

test_that("native planning and validation plots are available through the wrappers", {
  skip_if_not_installed("powerly")
  pair <- powerly_native_pair()
  for (step in 1:3) {
    plotted <- suppressMessages(suppressWarnings(plot(pair$wrapped, step = step)))
    expect_s3_class(plotted, "ggplot")
    expect_s3_class(plotted, "patchwork")
  }
  validation <- ValidateNetworkPower(pair$wrapped,
    replications = 5, seed = 88022, cores = 1, verbose = FALSE)
  plotted <- suppressMessages(suppressWarnings(plot(validation)))
  expect_s3_class(plotted, "ggplot")
  expect_s3_class(plotted, "patchwork")
})

test_that("unsupported powerly generator and estimator settings are rejected", {
  skip_if_not_installed("powerly")
  truth <- matrix(c(0, 0.3, 0.3, 0), 2)
  expect_error(NetworkPower(method = "powerly", model_matrix = truth,
    range_lower = 50, range_upper = 300, gamma = 0.25), "gamma")
  expect_error(NetworkPower(method = "powerly", model_matrix = truth,
    range_lower = 50, range_upper = 300, levels = 7), "levels")
  expect_error(NetworkPower(method = "powerly", model_matrix = truth,
    range_lower = 50, range_upper = 300, threshold = TRUE), "threshold")
})

test_that("nonattainment requires an explicit native validation sample size", {
  skip_if_not_installed("powerly")
  truth <- matrix(0, 5, 5)
  truth[cbind(1:4, 2:5)] <- 0.01
  truth <- truth + t(truth)
  plan <- NetworkPower(method = "powerly", model_matrix = truth,
    range_lower = 50, range_upper = 100, samples = 5, replications = 2,
    boots = 10, spline_df = 3, iterations = 1, tolerance = 5,
    target_metric = "sensitivity", target_value = 1,
    target_probability = 1, seed = 88031, cores = 1, verbose = FALSE)

  expect_false(plan$recommendation$reached)
  expect_true(is.na(plan$recommendation$recommended_n))
  expect_error(ValidateNetworkPower(plan, replications = 2,
    seed = 88032, cores = 1, verbose = FALSE), "sample|recommend|attain|reach")

  set.seed(88032)
  native <- powerly::validate(plan$fit, replications = 2, sample = 100,
                             cores = 1, verbose = FALSE)
  validation <- ValidateNetworkPower(plan, replications = 2, sample = 100,
    seed = 88032, cores = 1, verbose = FALSE)
  expect_equal(validation$fit$sample, native$sample)
  expect_equal(validation$fit$measures, native$measures)
  expect_equal(validation$summary$sample_size, 100)
})

test_that("decreasing search and native validation comparison directions remain explicit", {
  skip_if_not_installed("powerly")
  truth <- powerly_native_pair()$wrapped$true_network
  plan <- NetworkPower(method = "powerly", model = "ggm", model_matrix = truth,
    range_lower = 50, range_upper = 100, samples = 5, replications = 2,
    boots = 10, spline_df = 3, iterations = 1, tolerance = 5,
    increasing = FALSE, seed = 88041, cores = 1, verbose = FALSE)
  validation <- ValidateNetworkPower(plan, sample = 100, replications = 5,
    seed = 88042, cores = 1, verbose = FALSE)
  expect_identical(plan$recommendation$probability_comparison, "<=")
  expect_identical(validation$settings$search_probability_comparison, "<=")
  expect_identical(validation$settings$validation_probability_comparison, ">=")
  expect_equal(validation$summary$point_estimate_reaches_target,
    validation$fit$statistic >= plan$settings$target_probability)
  expect_match(validation$report, "different native criteria")
  expect_false(check_input(model = "power", method = "powerly", model_matrix = truth,
    range_lower = 50, range_upper = 100, levels = 7, quiet = TRUE)$ok)
})
