power_validation_chain <- function(strength = 0.3) {
  x <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  x[1, 2] <- x[2, 1] <- x[2, 3] <- x[3, 2] <- strength
  x
}

test_that("invalid native powerly targets and ranges fail before simulation", {
  skip_if_not_installed("powerly")
  controls <- list(method = "powerly", model_matrix = power_validation_chain(),
    range_lower = 50, range_upper = 300)
  for (metric in c("sensitivity", "specificity")) {
    for (value in c(-.01, 1.01)) {
      expect_error(do.call(NetworkPower, c(controls,
        list(target_metric = metric, target_value = value))), "target_value")
      checked <- do.call(check_input, c(list(model = "power", quiet = TRUE),
        controls, list(target_metric = metric, target_value = value)))
      expect_false(checked$ok)
      expect_match(paste(checked$errors, collapse = " "), "target_value")
    }
  }
  for (metric in c("mcc", "edge_weight_correlation")) {
    for (value in c(-1.01, 1.01)) {
      expect_error(do.call(NetworkPower, c(controls,
        list(target_metric = metric, target_value = value))), "target_value")
      checked <- do.call(check_input, c(list(model = "power", quiet = TRUE),
        controls, list(target_metric = metric, target_value = value)))
      expect_false(checked$ok)
      expect_match(paste(checked$errors, collapse = " "), "target_value")
    }
  }
  for (probability in c(-.01, 1.01)) {
    expect_error(do.call(NetworkPower, c(controls,
      list(target_probability = probability))), "target_probability")
    checked <- do.call(check_input, c(list(model = "power", quiet = TRUE),
      controls, list(target_probability = probability)))
    expect_false(checked$ok)
    expect_match(paste(checked$errors, collapse = " "), "target_probability")
  }
  expect_error(NetworkPower(method = "powerly", model_matrix = power_validation_chain(),
    range_lower = 200, range_upper = 100), "range_lower < range_upper")
  expect_error(NetworkPower(method = "powerly", model_matrix = power_validation_chain(),
    range_lower = 50, range_upper = 300, measure = "mcc", measure_value = 1.2), "target_value")
  expect_error(do.call(NetworkPower, c(controls, list(target_metric = "rmse"))))
  expect_error(do.call(NetworkPower, c(controls, list(statistic = "other"))), "power")
  expect_false(do.call(check_input, c(list(model = "power", quiet = TRUE),
    controls, list(statistic = "other")))$ok)
  expect_false(do.call(check_input, c(list(model = "power", quiet = TRUE),
    controls, list(measure = "sen", measure_value = -.4)))$ok)
  expect_true(do.call(check_input, c(list(model = "power", quiet = TRUE),
    controls, list(measure = "mcc", measure_value = -.4, statistic_value = .9)))$ok)
  expect_true(do.call(check_input, c(list(model = "power", quiet = TRUE),
    controls, list(measure_value = .4, statistic_value = .9)))$ok)
  expect_true(do.call(check_input, c(list(model = "power", quiet = TRUE),
    controls, list(target_metric = "mcc", measure_value = -.4,
      statistic_value = .9)))$ok)
})

test_that("powerly assumed networks must define a valid GGM precision matrix", {
  expect_silent(quicknet_power_validate_true_matrix(power_validation_chain()))
  invalid <- power_validation_chain()
  invalid[1, 2] <- .1
  expect_error(quicknet_power_validate_true_matrix(invalid), "symmetric")
  diag(invalid) <- .1
  expect_error(quicknet_power_validate_true_matrix(invalid), "zero diagonal")
  expect_error(quicknet_power_validate_true_matrix(matrix(c(0, 1.1, 1.1, 0), 2)),
    "positive-definite")
})

test_that("boundary attainment retains interval uncertainty despite zero plug-in MCSE", {
  full <- quicknet_power_binomial(4, 4)
  none <- quicknet_power_binomial(0, 4)
  expect_equal(full$mcse, 0)
  expect_lt(full$lower, .8)
  expect_equal(none$mcse, 0)
  expect_gt(none$upper, 0)
  expect_equal(c(full$lower, full$upper), as.numeric(binom.test(4, 4)$conf.int))
  expect_equal(c(none$lower, none$upper), as.numeric(binom.test(0, 4)$conf.int))
})

test_that("powerly recommendations follow the bootstrap median curve", {
  fit <- list(recommendation = c(`2.5%` = 120, `50%` = 150, `97.5%` = 190),
    step_2 = list(interpolation = list(x = 100:200, fitted = rep(.7, 101))),
    step_3 = list(ci = cbind(`50%` = seq(.5, 1.1, length.out = 101))))
  rec <- quicknet_power_powerly_recommendation(fit, .8)
  expect_true(rec$reached)
  expect_equal(rec$recommended_n, 150)
  expect_equal(rec$achieved_probability, .8)
  expect_equal(rec$fitted_probability, .7)
  expect_identical(rec$probability_source, "bootstrap_median_curve")
  expect_equal(c(rec$backend_n_lower, rec$backend_n_upper), c(120, 190))
  fit$recommendation[["50%"]] <- 200
  fit$step_3$ci[] <- .7
  expect_false(quicknet_power_powerly_recommendation(fit, .8)$reached)
  fit$step_2$spline <- list(basis = list(monotone = TRUE), solver = list(increasing = FALSE))
  rec <- quicknet_power_powerly_recommendation(fit, .8)
  expect_true(rec$reached)
  expect_identical(rec$probability_comparison, "<=")
})

test_that("a native true model matrix requires no unused generator controls", {
  skip_if_not_installed("powerly")
  truth <- matrix(c(0, .3, .3, 0), 2)
  received <- NULL
  backend <- function(...) NULL
  formals(backend) <- formals(powerly::powerly)
  body(backend) <- quote({
    received <<- list(matrix = model_matrix, extra = list(...))
    list(recommendation = c(`50%` = 200),
      range = list(partition = c(100, 200)),
      step_1 = list(true_model_parameters = model_matrix,
        measures = matrix(c(0, 1, 1, 1), 2), statistics = c(.5, 1)),
      step_2 = list(interpolation = list(x = 100:200, fitted = seq(.5, 1, length.out = 101))),
      step_3 = list(ci = cbind(`50%` = seq(.5, 1, length.out = 101))))
  })
  environment(backend) <- environment()
  local_mocked_bindings(powerly = backend, .package = "powerly")
  result <- NetworkPower(method = "powerly", model_matrix = truth, range_lower = 100, range_upper = 200)
  expect_equal(received$matrix, truth)
  expect_length(received$extra, 0)
  expect_equal(result$true_network, truth)
  expect_equal(result$settings$generated_nodes, 2)
  expect_equal(result$settings$generated_density, 1)
  expect_equal(result$settings$target_value, .6)
  expect_equal(result$settings$backend_data_levels, 5)
  expect_equal(result$summary$replications, c(2, 2))
  expect_match(result$settings$denominator_policy, "replaces_NA")
  undefined <- NetworkPower(method = "powerly", model_matrix = truth,
    range_lower = 100, range_upper = 200, measure = "mcc")
  expect_false(undefined$settings$target_defined_for_truth)
  expect_false(undefined$recommendation$reached)
  expect_true(is.na(undefined$recommendation$recommended_n))
  expect_match(undefined$report, "undefined")
})

test_that("native powerly summary keeps zero-valued recovery in the denominator", {
  measures <- cbind(c(1, 0, 0), c(1, .7, 0))
  backend <- list(range = list(partition = c(50, 100)),
    step_1 = list(measures = measures, statistics = c(1 / 3, 2 / 3)))
  result <- quicknet_power_powerly_summary(backend, "mcc", .6)
  expect_equal(result$replications, c(3, 3))
  expect_equal(result$finite_metric_replications, c(3, 3))
  expect_equal(result$achieved_replications, c(1, 2))
  expect_equal(result$achieved_probability, c(1 / 3, 2 / 3))
  expect_equal(c(result$probability_ci_lower[[1]], result$probability_ci_upper[[1]]),
    as.numeric(binom.test(1, 3)$conf.int))
  expect_error(quicknet_power_powerly_summary(list(range = list(partition = 50),
    step_1 = list(statistics = c(.5, .8))), "mcc", .6), "mismatched")
})
