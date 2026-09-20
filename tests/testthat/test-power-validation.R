power_validation_chain <- function(strength = 0.3) {
  x <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  x[1, 2] <- x[2, 1] <- x[2, 3] <- x[3, 2] <- strength
  x
}

test_that("correlation recovery uses marginal truth and preserves the generating network", {
  local_mocked_bindings(quicknet_power_simulate_estimate = function(sample_size, covariance, estimator, gamma) {
    diag(covariance) <- 0
    covariance
  })
  x <- NetworkPower(nodes = 3, density = 2/3, edge_strength = c(.3, .3),
    positive = 1, estimator = "correlation", sample_sizes = 20, replications = 3,
    target_metric = "rmse", target_value = 0, seed = 6491)
  # The missing edge of a three-node partial-correlation chain has a nonzero
  # population marginal correlation, derived by inverting its precision.
  sigma <- cov2cor(solve(diag(3) - x$generating_network)); diag(sigma) <- 0
  expect_equal(unname(x$true_network), unname(sigma))
  expect_gt(sum(abs(x$true_network - x$generating_network)), .01)
  expect_equal(x$results$rmse, rep(0, 3))
  expect_identical(x$settings$estimand, "marginal_correlation")
  expect_match(x$report, "conditional on one fixed")
})

test_that("Monte Carlo records actual PD shrinkage and generated edge strengths", {
  x <- NetworkPower(nodes = 5, density = 1, positive = 1, edge_strength = c(.8, .8),
    sample_sizes = 60, replications = 1, target_metric = "rmse", seed = 7392)
  expect_lt(x$settings$positive_definite_scale, 1)
  expect_equal(x$settings$generated_edge_strength, rep(.8 * x$settings$positive_definite_scale, 2))
  expect_equal(x$settings$edge_strength, c(.8, .8))
  expect_gt(min(eigen(diag(5) - x$generating_network, symmetric = TRUE)$values), 0)
  expect_equal(x$settings$generated_density, 1)
  expect_equal(x$true_network, x$generating_network)
  expect_match(x$report, "positive-definite scaling factor")
})

test_that("invalid targets and singular unregularized designs fail before simulation", {
  for (metric in c("sensitivity", "specificity")) {
    for (value in c(-.01, 1.01)) expect_error(NetworkPower(target_metric = metric, target_value = value), "target_value")
  }
  for (metric in c("mcc", "edge_weight_correlation")) {
    for (value in c(-1.01, 1.01)) expect_error(NetworkPower(target_metric = metric, target_value = value), "target_value")
  }
  expect_error(NetworkPower(target_metric = "rmse", target_value = -.1), "target_value")
  expect_error(NetworkPower(nodes = 8, sample_sizes = 8, estimator = "partial"), "exceed nodes")
  expect_error(NetworkPower(method = "powerly", model_matrix = power_validation_chain(),
    range_lower = 200, range_upper = 100), "range_lower < range_upper")
  expect_error(NetworkPower(method = "powerly", model_matrix = power_validation_chain(),
    range_lower = 50, range_upper = 300, measure = "mcc", measure_value = 1.2), "target_value")
})

test_that("failed fits and undefined metrics are separate and retained in the denominator", {
  graph <- power_validation_chain()
  ok <- quicknet_power_recovery_metrics(graph, graph, 1e-10)
  undefined <- quicknet_power_recovery_metrics(graph, matrix(.2, 3, 3), 1e-10)
  add <- function(row) {
    row$sample_size <- 60; row$replication <- 1; row$gamma <- .5
    row$estimator <- "EBICglasso"; row$estimated_nonzero_edges <- 2
    row$failed <- FALSE; row$error_message <- NA_character_; row
  }
  rows <- rbind(add(ok), add(undefined), quicknet_power_empty_metric(60, 3, .5, "EBICglasso"))
  summary <- quicknet_power_summary(rows, "mcc", .6)
  expect_equal(summary$achieved_probability, 1 / 3)
  expect_equal(summary$failed_replications, 1)
  expect_equal(summary$undefined_target_replications, 1)
  expect_equal(summary$valid_target_replications, 1)
  expect_equal(c(summary$probability_ci_lower, summary$probability_ci_upper),
               as.numeric(binom.test(1, 3)$conf.int))
  none <- quicknet_power_summary(rows[2, ], "mcc", 0)
  expect_false(quicknet_power_recommend(none, 0)$reached)
})

test_that("boundary attainment retains interval uncertainty despite zero plug-in MCSE", {
  full <- quicknet_power_binomial(4, 4)
  none <- quicknet_power_binomial(0, 4)
  expect_equal(full$mcse, 0)
  expect_lt(full$lower, .8)
  expect_equal(none$mcse, 0)
  expect_gt(none$upper, 0)
  summary <- data.frame(sample_size = 50, achieved_probability = 1, valid_target_replications = 4,
    probability_mcse = full$mcse, probability_ci_lower = full$lower, probability_ci_upper = full$upper)
  rec <- quicknet_power_recommend(summary, .8)
  expect_true(rec$reached)
  expect_true(rec$at_lower_boundary)
  expect_false(rec$lower_bound_supports_target)
  expect_match(quicknet_power_report_text(rec, "mcc", .6, .8), "not adjusted for selecting")
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
})

test_that("partial backend failures keep their cause and never leave the probability denominator", {
  calls <- 0L
  local_mocked_bindings(quicknet_power_simulate_estimate = function(sample_size, covariance, estimator, gamma) {
    calls <<- calls + 1L
    if (calls == 1L) stop("deliberate backend failure")
    if (calls == 2L) return(matrix(NaN, nrow(covariance), ncol(covariance)))
    diag(covariance) <- 0
    covariance
  })
  expect_warning(result <- NetworkPower(nodes = 3, density = 2/3, sample_sizes = 30,
    replications = 3, estimator = "correlation", target_metric = "rmse", target_value = 0,
    seed = 3231), "2 of 3 Monte Carlo")
  expect_equal(result$summary$failed_replications, 2)
  expect_equal(result$summary$undefined_target_replications, 0)
  expect_equal(result$summary$achieved_probability, 1 / 3)
  expect_match(result$results$error_message[[1]], "deliberate backend failure")
  expect_match(result$results$error_message[[2]], "invalid network")
  expect_false(result$results$failed[[3]])
})
