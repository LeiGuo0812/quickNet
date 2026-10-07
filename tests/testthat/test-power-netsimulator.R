netsimulator_reference_graph <- function() {
  graph <- matrix(0, 5, 5)
  graph[1, 2] <- graph[2, 1] <- .25
  graph[2, 3] <- graph[3, 2] <- .35
  graph[3, 4] <- graph[4, 3] <- -.20
  graph[4, 5] <- graph[5, 4] <- .15
  graph[2, 5] <- graph[5, 2] <- .10
  graph
}

test_that("netSimulator preserves native recovery values and varied estimation conditions", {
  skip_if_not_installed("bootnet")
  graph <- netsimulator_reference_graph()
  args <- list(default = "EBICglasso", corMethod = "cor", tuning = c(.25, .5),
               nCores = 1, moreOutput = list(squared_error = function(est, truth) {
                 mean((est - truth)^2)
               }))
  set.seed(606)
  capture.output(native <- do.call(bootnet::netSimulator,
    c(list(input = graph, nCases = c(80, 120), nReps = 2), args)))
  capture.output(wrapped <- do.call(NetworkPower,
    c(list(model_matrix = graph, sample_sizes = c(80, 120), replications = 2,
           seed = 606), args)))

  expect_s3_class(wrapped, "quicknet_power")
  expect_s3_class(wrapped$fit, "netSimulator")
  expect_identical(wrapped$fit, native)
  expect_identical(wrapped$results, native)
  expect_identical(wrapped$raw, native)
  expect_true(all(c("strength", "closeness", "betweenness", "ExpectedInfluence",
                    "bias", "squared_error", "error", "tuning") %in% names(wrapped$raw)))
  expect_equal(nrow(wrapped$raw), 8)
  capture.output(native_summary <- summary(native))
  expect_identical(wrapped$summary, native_summary)
  capture.output(native_digits <- summary(native, digits = 3))
  capture.output(wrapped_digits <- summary(wrapped, digits = 3))
  expect_identical(wrapped_digits, native_digits)
  plotting <- list(yvar = c("strength", "ExpectedInfluence"), color = "tuning",
                   print = FALSE, style = "basic")
  native_plot <- do.call(plot, c(list(x = native), plotting))
  wrapped_plot <- do.call(plot, c(list(x = wrapped), plotting))
  expect_s3_class(wrapped_plot, "ggplot")
  expect_identical(wrapped_plot$data, native_plot$data)
  expect_identical(wrapped$recommendation$status, "not_applicable")
  expect_true(is.na(wrapped$recommendation$recommended_n))
  expect_true(is.na(wrapped$recommendation$reached))
  expect_match(wrapped$report, "does not automatically recommend")
  expect_identical(wrapped$settings$backend_args$moreOutput, args$moreOutput)
  report <- quicknet_report(wrapped)
  expect_s3_class(report, "quicknet_report")
  expect_identical(report$status, "not_applicable")
  expect_identical(report$summary$sensitivity, wrapped$summary$sensitivity)
})

test_that("ordinal netSimulator uses the original generator and estimator", {
  skip_if_not_installed("bootnet")
  graph <- netsimulator_reference_graph()
  generator <- bootnet::ggmGenerator(ordinal = TRUE, nLevels = 5)
  args <- list(default = "EBICglasso", dataGenerator = generator,
               corMethod = "cor_auto", tuning = .5, nCores = 1)
  set.seed(607)
  capture.output(native <- do.call(bootnet::netSimulator,
    c(list(input = graph, nCases = c(100, 150), nReps = 1), args)))
  capture.output(wrapped <- quicknet_power_netsimulator(
    graph, sample_sizes = c(100, 150), replications = 1, seed = 607,
    native_args = args))
  expect_identical(wrapped$fit, native)
  expect_false(any(wrapped$raw$error))
  expect_identical(wrapped$settings$backend_args$dataGenerator, generator)
  expect_identical(wrapped$settings$backend_args$corMethod, "cor_auto")
})

test_that("Ising netSimulator preserves the graph and intercepts without GGM constraints", {
  skip_if_not_installed("bootnet")
  skip_if_not_installed("IsingFit")
  skip_if_not_installed("IsingSampler")
  # I - graph is indefinite: it is an Ising interaction matrix, not a GGM.
  graph <- matrix(0, 3, 3)
  graph[1, 2] <- graph[2, 1] <- 1.1
  graph[2, 3] <- graph[3, 2] <- .3
  input <- list(graph = graph, intercepts = c(-.5, -.5, -.5))
  args <- list(default = "IsingFit", nCores = 1)
  set.seed(609)
  capture.output(native <- do.call(bootnet::netSimulator,
    c(list(input = input, nCases = 100, nReps = 1), args)))
  capture.output(wrapped <- quicknet_power_netsimulator(input,
    sample_sizes = 100, replications = 1, seed = 609, native_args = args))
  expect_identical(wrapped$fit, native)
  expect_false(any(wrapped$raw$error))
  expect_identical(wrapped$model, "ising")
  expect_identical(wrapped$settings$backend_args$input, input)
  expect_identical(wrapped$true_network, graph)
})

test_that("native argument names and quickNet aliases resolve without hidden defaults", {
  skip_if_not_installed("bootnet")
  graph <- netsimulator_reference_graph()
  received <- NULL
  backend <- function(...) {
    received <<- list(...)
    structure(data.frame(nCases = 25, rep = 1, id = 1, default = "EBICglasso",
      sensitivity = 1, specificity = 1, correlation = 1, strength = 1,
      closeness = 1, betweenness = 1, ExpectedInfluence = 1,
      bias = 0, correctModel = TRUE, MaxFalseEdgeWidth = NA_real_,
      error = FALSE, errorMessage = NA_character_),
      class = c("netSimulator", "data.frame"))
  }
  local_mocked_bindings(netSimulator = backend, .package = "bootnet")
  capture.output(wrapped <- quicknet_power_netsimulator(native_args = list(
    input = graph, nCases = 25, nReps = 1, default = "EBICglasso", nCores = 1)))
  expect_identical(received, wrapped$settings$backend_args)
  expect_false("dataGenerator" %in% names(received))
  expect_equal(wrapped$settings$sample_sizes, 25)
  expect_equal(wrapped$settings$replications, 1)
  capture.output(alias <- SampleSize(model_matrix = graph, sample_sizes = 25,
    replications = 1, gamma = .3, threshold = TRUE, nCores = 1))
  expect_identical(alias$method, "netSimulator")
  expect_identical(received$tuning, .3)
  expect_identical(received$threshold, TRUE)
  expect_false("gamma" %in% names(received))
  expect_false("default" %in% names(received))
  expect_false("dataGenerator" %in% names(received))

  expect_error(quicknet_power_netsimulator(), "assumed network")
  expect_error(quicknet_power_netsimulator(graph, native_args = list(input = graph)), "not both")
  expect_error(quicknet_power_netsimulator(graph, sample_sizes = 25,
    native_args = list(nCases = 30)), "not both")
  expect_error(quicknet_power_netsimulator(graph, replications = 1,
    native_args = list(nReps = 2)), "not both")
  expect_error(quicknet_power_netsimulator(graph,
    native_args = list(1)), "unique, non-empty names")
  expect_error(quicknet_power_netsimulator_args(graph, sample_sizes = c(30, NA)), "nCases")
  expect_error(quicknet_power_netsimulator_args(graph, replications = 0), "nReps")
  expect_error(quicknet_power_netsimulator_args(graph, native_args = list(nCores = c(1, 2))), "nCores")
  expect_error(NetworkPower(model_matrix = graph, target_probability = .8), "do not apply")
  expect_error(NetworkPower(model_matrix = graph, gamma = .5, tuning = .25), "not both")
})

test_that("metadata records the estimator resolved by native missing-argument rules", {
  skip_if_not_installed("bootnet")
  graph <- netsimulator_reference_graph()
  capture.output(automatic <- NetworkPower(model_matrix = graph,
    nCases = 30, nReps = 1, seed = 610))
  expect_identical(automatic$settings$estimator, unique(automatic$fit$default))
  expect_identical(automatic$settings$estimator, "EBICglasso")
  expect_false("default" %in% names(automatic$settings$backend_args))

  # With a generator and varied estimator arguments, native default is 'none'.
  # Fixed estimator functions belong in moreArgs, not in the condition grid.
  estimator <- function(data, ...) graph
  capture.output(custom <- NetworkPower(model_matrix = graph,
    nCases = 30, nReps = 1, seed = 610,
    dataGenerator = bootnet::ggmGenerator(), tuning = .2,
    moreArgs = list(fun = estimator)))
  expect_identical(custom$settings$estimator, unique(custom$fit$default))
  expect_identical(custom$settings$estimator, "none")
  expect_false(any(custom$raw$error))
  expect_identical(custom$model, "custom")
})

test_that("netSimulator input checking validates controls without running simulations", {
  skip_if_not_installed("bootnet")
  local_mocked_bindings(netSimulator = function(...) {
    stop("Input checking must not run a simulation")
  }, .package = "bootnet")
  graph <- netsimulator_reference_graph()
  valid <- check_input(model = "power", model_matrix = graph,
    sample_sizes = c(100, 250), replications = 30, quiet = TRUE)
  expect_true(valid$ok)
  threshold <- check_input(model = "power", model_matrix = graph,
    sample_sizes = 100, replications = 30, threshold = TRUE, quiet = TRUE)
  expect_true(threshold$ok)
  generated <- check_input(model = "power", input = function() {
    stop("Input checking must not draw a true network")
  }, nCases = c(100, 250), nReps = 30, quiet = TRUE)
  expect_true(generated$ok)
  absent <- check_input(model = "power", quiet = TRUE)
  expect_false(absent$ok)
  expect_match(absent$errors, "assumed network")
  conflict <- check_input(model = "power", model_matrix = graph,
    sample_sizes = 100, nCases = 250, quiet = TRUE)
  expect_false(conflict$ok)
  expect_match(conflict$errors, "not both")
  inactive <- check_input(model = "power", model_matrix = graph,
    target_probability = .8, quiet = TRUE)
  expect_false(inactive$ok)
  expect_match(inactive$errors, "not applicable")
})

test_that("native lists, network functions and all-failed runs remain inspectable", {
  skip_if_not_installed("bootnet")
  graph <- netsimulator_reference_graph()
  # A custom generator/estimator isolates preservation of the original list,
  # including Ising intercepts, without imposing a GGM precision restriction.
  input <- list(graph = graph, intercepts = rep(-.5, 5),
                thresholds = rep(list(c(-1, -.25, .25, 1)), 5))
  generator <- function(n, input) {
    matrix(rnorm(n * ncol(input$graph)), n, ncol(input$graph))
  }
  estimator <- function(data) graph
  args <- list(input = input, nCases = 30, nReps = 1,
               default = "none", dataGenerator = generator,
               moreArgs = list(fun = estimator))
  capture.output(wrapped <- quicknet_power_netsimulator(seed = 608,
                                                       native_args = args))
  expect_identical(wrapped$settings$backend_args$input, input)
  expect_equal(wrapped$raw$correlation, 1)
  expect_identical(wrapped$true_network, graph)

  args$input <- function() input
  capture.output(generated <- quicknet_power_netsimulator(seed = 608,
                                                         native_args = args))
  expect_null(generated$true_network)
  expect_identical(generated$settings$reference_type, "network_generator")
  expect_equal(generated$raw$correlation, 1)

  args$moreArgs$fun <- function(data) stop("deliberate native fitting error")
  capture.output(failed <- quicknet_power_netsimulator(seed = 608,
                                                      native_args = args))
  expect_true(all(failed$raw$error))
  expect_match(failed$raw$errorMessage, "deliberate native fitting error")
  expect_null(failed$summary)
  expect_match(failed$summary_error, "All simulations resulted in errors")
})
