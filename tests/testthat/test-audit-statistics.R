test_that("MTD matches the temporal-derivative normalization in Shine Eq. 2", {
  set.seed(712)
  x <- cbind(cumsum(rnorm(40)), cumsum(rnorm(40)))
  derivative <- apply(x, 2, diff)
  normalized <- sweep(derivative, 2, apply(derivative, 2, sd), "/")
  expected <- normalized[, 1] * normalized[, 2]
  set.seed(713)
  result <- MTD.No.Smooth.Test(x, nperm = 19)
  expect_equal(result$coupling[, 1, 2], unname(expected))
  expect_equal(result$coupling_mean, mean(expected))
  set.seed(713)
  null_values <- replicate(19, {
    d <- diff(sample(x[, 2]))
    mean(normalized[, 1] * d / sd(d))
  })
  expect_equal(result$p.value, (1 + sum(abs(null_values) >= abs(mean(expected)))) / 20)
  expect_error(MTD.No.Smooth.Test(cbind(1:20, (1:20)^2), nperm = 1), "temporal derivatives")
})

test_that("MCC does not overflow with thousands of edges", {
  graph <- matrix(0, 100, 100)
  graph[upper.tri(graph)] <- rep(c(0, 0.3), length.out = sum(upper.tri(graph)))
  graph <- graph + t(graph)
  expect_no_warning(perfect <- quicknet_power_recovery_metrics(graph, graph, 1e-10))
  expect_equal(perfect$mcc, 1)
  wrong <- matrix(0.3, 100, 100) - graph
  diag(wrong) <- 0
  expect_equal(quicknet_power_recovery_metrics(graph, wrong, 1e-10)$mcc, -1)
  expect_error(NetworkPower(threshold = -0.1), "threshold")
})

test_that("powerly boundary fallback is not reported as a successful recommendation", {
  backend <- list(recommendation = c(`50%` = 150),
                  step_2 = list(interpolation = list(x = 50:150, fitted = rep(0, 101))))
  result <- quicknet_power_powerly_recommendation(backend, .8)
  expect_false(result$reached)
  expect_true(is.na(result$recommended_n))
  expect_equal(result$backend_recommended_n, 150)
  expect_equal(result$achieved_probability, 0)
  expect_true(result$at_upper_boundary)
  backend$step_2$interpolation$fitted <- seq(0, 1, length.out = 101)
  result <- quicknet_power_powerly_recommendation(backend, .8)
  expect_true(result$reached)
  expect_equal(result$recommended_n, 150)
})

test_that("NCT accepts an estimateNetwork object in either argument", {
  estimator <- function(x, verbose = FALSE) {
    graph <- cor(x)
    diag(graph) <- 0
    graph
  }
  x <- mtcars[, 1:4]
  fitted <- structure(list(data = x, estimator = estimator, arguments = list()),
                      class = "bootnetResult")
  compare <- function(a, b) NCT_gl(a, b, it = 2, test.centrality = FALSE,
                                  progressbar = FALSE, verbose = FALSE)
  expect_equal(compare(x, fitted)$nwinv.real, 0)
  expect_equal(compare(fitted, x)$nwinv.real, 0)
  expect_equal(compare(fitted, fitted)$nwinv.real, 0)
})

test_that("NCT selected-edge results keep numeric p values and integer indices", {
  estimator <- function(x) {
    graph <- cor(x)
    diag(graph) <- 0
    graph
  }
  compare <- function(edges) NCT_gl(mtcars[, 1:4], mtcars[, 1:4]^2,
    estimator = estimator, edges = edges, test.edges = TRUE,
    it = 3, test.centrality = FALSE, progressbar = FALSE)
  result <- compare(list(c("mpg", "cyl")))
  expect_type(result$einv.pvals$`p-value`, "double")
  expect_equal(result$einv.pvals$`p-value`,
               (1 + sum(result$einv.perm[, 1] >= result$einv.real[[1]])) / 4)
  expect_error(compare(list(c(1.5, 2))), "distinct valid variables")
})

test_that("Gaussian conditioning agrees with the analytic covariance formula", {
  covariance <- matrix(c(1, .4, .2, .4, 1, .3, .2, .3, 1), 3,
                       dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  state <- quicknet_perturb_conditioned_state(solve(covariance), c(a = -1))
  expect_equal(unname(state), c(-1, -.4, -.2))
  state <- quicknet_perturb_conditioned_state(solve(covariance), c(a = -1, b = -.5))
  expected_c <- covariance["c", c("a", "b")] %*%
    solve(covariance[c("a", "b"), c("a", "b")], c(-1, -.5))
  expect_equal(unname(state[["c"]]), as.numeric(expected_c))
  expect_error(quicknet_perturb_conditioned_state(solve(covariance), c(a = NA_real_)), "target_values")
})

test_that("perturbation parameters cannot silently produce invalid simulations", {
  fit <- quicknet_fit_cross_sectional(mtcars[, 1:4], model = "partial")
  expect_error(Perturbation(fit, method = "knockdown", remaining_strength = 2), "fraction")
  expect_error(Perturbation(fit, dose = Inf), "dose")
  expect_error(Perturbation(fit, targets = character()), "at least one node")
  expect_error(Perturbation(fit, targets = list(c("mpg", "mpg"))), "unique")
  expect_error(Perturbation(fit, method = "edge_block", spillover_nodes = "absent"), "Unknown node")
  expect_error(Perturbation(fit, method = "edge_block", threshold = 2), "No edges")
  graph <- matrix(0, 2, 2)
  thresholds <- c(a = 0, b = 0)
  expect_error(quicknet_perturb_ising_gibbs(graph, thresholds, 0, 2, 1, 1), "n_samples")
  expect_error(quicknet_perturb_ising_gibbs(graph, thresholds, 10, 2, 0, 1), "thinning")
  expect_error(quicknet_perturb_ising_gibbs(graph, thresholds, 10, -1, 1, 1), "burnin")
})

test_that("Ising Gibbs marginals match independent-node probabilities", {
  graph <- matrix(0, 2, 2)
  thresholds <- c(a = qlogis(.2), b = qlogis(.8))
  samples <- quicknet_perturb_ising_gibbs(graph, thresholds, 6000, 10, 1, 714)
  expect_equal(unname(colMeans(samples)), c(.2, .8), tolerance = .025)
  fit <- structure(list(model = "ising", graph = graph,
                        nodes = data.frame(node = c("V1", "V2"), threshold = c(0, 0))),
                   class = "quicknet_fit")
  expect_s3_class(Perturbation(fit, seed = NULL, n_samples = 10, burnin = 0,
                              thinning = 1), "quicknet_perturbation")
})

test_that("ranking plots keep distinct perturbation conditions in separate bars", {
  fit <- quicknet_fit_cross_sectional(mtcars[, 1:4], model = "partial")
  for (result in list(Perturbation(fit, targets = "mpg", dose = c(.25, .5)),
                      Perturbation(fit, method = "knockout", targets = "mpg"))) {
    plot <- get_perturbation_plot(result, type = "rank")
    bars <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_equal(length(unique(bars$x)), nrow(result$metrics))
    expect_equal(sort(bars$y), sort(result$metrics$burden_reduction))
    expect_error(get_perturbation_plot(result, top_n = 0), "top_n")
  }
})

test_that("Stability requests an actual centrality difference plot", {
  calls <- list()
  testthat::local_mocked_bindings(
    quicknet_bootstrap_edge_stability = function(...) data.frame(),
    quicknet_case_drop_centrality_stability = function(...) data.frame(),
    bootnet = function(...) structure(list(), class = "audit_boot"),
    corStability = function(...) 1,
    .package = "quickNet"
  )
  testthat::local_mocked_bindings(plot = function(...) {
    calls[[length(calls) + 1]] <<- list(...)
    NULL
  }, .package = "base")
  fit <- structure(list(model = "EBICglasso", fit = list()), class = "quicknet_fit")
  invisible(Stability(fit, nboot = 1))
  strength_calls <- Filter(function(x) length(x) >= 2 && identical(x[[2]], "strength"), calls)
  expect_length(strength_calls, 1)
  expect_identical(strength_calls[[1]]$plot, "difference")
})
