make_nira_test_fit <- function() {
  node_names <- c("A", "B", "C", "D")
  graph <- matrix(
    c(
      0, 0.30, -0.10, 0.05,
      0.30, 0, 0.20, -0.05,
      -0.10, 0.20, 0, 0.25,
      0.05, -0.05, 0.25, 0
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(node_names, node_names)
  )
  set.seed(301)
  data <- as.data.frame(
    matrix(stats::rbinom(400, size = 1, prob = 0.5), nrow = 100)
  )
  names(data) <- node_names
  thresholds <- c(A = -0.9, B = -0.2, C = 0.35, D = 0.95)
  nodes <- data.frame(
    node = node_names,
    network = "default",
    strength = rowSums(abs(graph)),
    expected_influence = rowSums(graph),
    degree_nonzero = rowSums(graph != 0),
    threshold = as.numeric(thresholds),
    stringsAsFactors = FALSE
  )
  quicknet_fit(
    model = "ising",
    data = data,
    networks = list(default = graph),
    nodes = nodes,
    fit = list(thresholds = thresholds),
    meta = list(AND = TRUE, data_type = "cross_sectional")
  )
}

run_small_nira <- function(fit = make_nira_test_fit(), ...) {
  suppressWarnings(NIRA(
    fit,
    n_samples = 40L,
    run_moderation = FALSE,
    run_permutation = FALSE,
    run_stability = FALSE,
    seed = 44L,
    engine = "native",
    ...
  ))
}

test_that("NIRA accepts only supported Ising quicknet fits", {
  fit <- make_nira_test_fit()
  expect_s3_class(run_small_nira(fit), "quicknet_nira")

  non_ising <- fit
  non_ising$model <- "EBICglasso"
  expect_error(run_small_nira(non_ising), "only.*Ising")
  expect_error(NIRA(list()), "quicknet_fit")
})

test_that("NIRA extracts confirmatory Ising beta and thresholds", {
  skip_if_not_installed("psychonetrics")
  set.seed(302)
  data <- as.data.frame(
    matrix(stats::rbinom(450, size = 1, prob = 0.5), nrow = 150)
  )
  names(data) <- c("A", "B", "C")
  fit <- ConfirmatoryNet(data, model = "ising", vars = names(data))
  result <- suppressWarnings(NIRA(
    fit,
    n_samples = 20,
    run_moderation = FALSE,
    run_permutation = FALSE,
    run_stability = FALSE,
    engine = "native"
  ))
  expected_beta <- as.numeric(
    psychonetrics::getmatrix(fit$fit$model, "beta")
  )
  expect_s3_class(result, "quicknet_nira")
  expect_equal(result$provenance$beta, expected_beta)
  expect_equal(
    result$baseline$thresholds,
    stats::setNames(fit$nodes$threshold, fit$nodes$node)
  )
})

test_that("NIRA consumes an exploratory quickNet Ising fit", {
  set.seed(303)
  data <- as.data.frame(
    matrix(stats::rbinom(600, size = 1, prob = 0.5), nrow = 150)
  )
  names(data) <- c("A", "B", "C", "D")
  fit <- quickNet(
    data,
    model = "ising",
    gamma = 0.25,
    AND = TRUE,
    pie = FALSE
  )
  result <- suppressWarnings(NIRA(
    fit,
    n_samples = 20,
    run_moderation = FALSE,
    run_permutation = FALSE,
    run_stability = FALSE,
    engine = "native"
  ))
  expect_s3_class(result, "quicknet_nira")
  expect_identical(result$model, "ising")
  expect_identical(result$interventions$node, names(data))
})

test_that("NIRA rejects non-binary and missing analysis data", {
  fit <- make_nira_test_fit()
  fit$data$A[[1L]] <- 2
  expect_error(run_small_nira(fit), "0/1")

  fit <- make_nira_test_fit()
  fit$data$A[[1L]] <- NA_real_
  expect_error(run_small_nira(fit), "does not impute")
})

test_that("NIRA rejects invalid thresholds and threshold names", {
  fit <- make_nira_test_fit()
  fit$nodes$threshold[[1L]] <- Inf
  expect_error(run_small_nira(fit), "finite")

  fit <- make_nira_test_fit()
  fit$nodes$node <- rev(fit$nodes$node)
  expect_error(run_small_nira(fit), "exactly match")

  fit <- make_nira_test_fit()
  fit$nodes$threshold <- 1
  fit$fit$thresholds[] <- 1
  expect_error(run_small_nira(fit), "sd\\(thresholds\\)")
})

test_that("NIRA rejects invalid graph dimensions, symmetry, and names", {
  fit <- make_nira_test_fit()
  fit$graph[1, 2] <- fit$graph[1, 2] + 0.1
  expect_error(run_small_nira(fit), "symmetric")

  fit <- make_nira_test_fit()
  fit$graph <- fit$graph[-1, -1]
  expect_error(run_small_nira(fit), "square matrix matching")

  fit <- make_nira_test_fit()
  rownames(fit$graph) <- rev(rownames(fit$graph))
  expect_error(run_small_nira(fit), "does not silently reorder")
})

test_that("NIRA computes the threshold delta exactly", {
  fit <- make_nira_test_fit()
  result <- run_small_nira(
    fit,
    amount_of_SDs_perturbation = 1.75
  )
  expected <- 1.75 * stats::sd(fit$nodes$threshold)
  expect_equal(result$settings$threshold_delta, expected, tolerance = 0)
  expect_equal(unique(result$interventions$threshold_delta), expected, tolerance = 0)
})

test_that("NIRA rejects non-finite threshold perturbations before simulation", {
  fit <- make_nira_test_fit()
  large_thresholds <- c(A = -2, B = 0, C = 2, D = 4)
  fit$nodes$threshold <- unname(large_thresholds)
  fit$fit$thresholds <- large_thresholds
  expect_error(
    run_small_nira(
      fit,
      amount_of_SDs_perturbation = .Machine$double.xmax
    ),
    "threshold delta must remain finite"
  )

  fit <- make_nira_test_fit()
  expect_error(
    run_small_nira(
      fit,
      amount_of_SDs_perturbation = 1e-300
    ),
    "change every target threshold"
  )
})

test_that("literature and native engines use the exact Ising 0/1 parameterization", {
  graph <- matrix(
    c(
      0, 0.4, -0.2,
      0.4, 0, 0.3,
      -0.2, 0.3, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  thresholds <- c(-0.6, 0.15, 0.8)
  beta <- 1.2
  states <- as.matrix(expand.grid(rep(list(0:1), 3)))

  for (state_index in seq_len(nrow(states))) {
    state_zero <- states[state_index, ]
    state_zero[[1L]] <- 0
    state_one <- state_zero
    state_one[[1L]] <- 1
    probability_zero <- IsingSampler::IsingStateProb(
      state_zero, graph, thresholds, beta
    )
    probability_one <- IsingSampler::IsingStateProb(
      state_one, graph, thresholds, beta
    )
    exact_conditional <- probability_one /
      (probability_zero + probability_one)
    expected_conditional <- stats::plogis(
      beta * (thresholds[[1L]] + sum(graph[1L, ] * state_zero))
    )
    expect_equal(exact_conditional, expected_conditional, tolerance = 1e-12)
  }

  unnormalized <- exp(vapply(seq_len(nrow(states)), function(index) {
    state <- states[index, ]
    beta * (
      sum(thresholds * state) +
        sum(graph[upper.tri(graph)] *
              outer(state, state)[upper.tri(graph)])
    )
  }, numeric(1)))
  exact_probability <- unnormalized / sum(unnormalized)
  exact_means <- colSums(states * exact_probability)

  native_stream <- quickNet:::quicknet_nira_make_streams(801, 1)[[1L]]
  native <- quickNet:::quicknet_nira_simulate_condition(
    graph,
    stats::setNames(thresholds, c("A", "B", "C")),
    n_samples = 8000,
    engine = "native",
    stream = native_stream,
    beta = beta
  )
  literature_stream <- quickNet:::quicknet_nira_make_streams(802, 1)[[1L]]
  literature <- quickNet:::quicknet_nira_simulate_condition(
    graph,
    stats::setNames(thresholds, c("A", "B", "C")),
    n_samples = 8000,
    engine = "literature",
    stream = literature_stream,
    beta = beta
  )
  expect_lte(max(abs(colMeans(native) - exact_means)), 0.03)
  expect_lte(max(abs(colMeans(literature) - exact_means)), 0.03)
  expect_lte(max(abs(colMeans(native) - colMeans(literature))), 0.04)
})

test_that("engine iterations are validated, recorded, and control simulation", {
  result <- run_small_nira(engine_iterations = 7L)
  expect_identical(result$settings$engine_iterations, 7L)
  expect_identical(result$provenance$engine_iterations, 7L)
  expect_true(any(grepl(
    "convergence diagnostic",
    result$warnings,
    fixed = TRUE
  )))

  node_names <- LETTERS[1:4]
  graph <- matrix(
    4,
    nrow = 4L,
    ncol = 4L,
    dimnames = list(node_names, node_names)
  )
  diag(graph) <- 0
  thresholds <- stats::setNames(rep(-5.7, 4L), node_names)
  stream <- quickNet:::quicknet_nira_make_streams(21L, 1L)[[1L]]
  one_sweep <- quickNet:::quicknet_nira_simulate_condition(
    graph, thresholds, 500L, "native", stream, 1, n_iter = 1L
  )
  twenty_sweeps <- quickNet:::quicknet_nira_simulate_condition(
    graph, thresholds, 500L, "native", stream, 1, n_iter = 20L
  )
  expect_false(identical(one_sweep, twenty_sweeps))

  expect_error(
    run_small_nira(engine_iterations = 0L),
    "engine_iterations"
  )
  expect_error(
    run_small_nira(n_samples = .Machine$integer.max + 1),
    "n_samples"
  )
  expect_error(
    run_small_nira(moderation_nboot = .Machine$integer.max),
    "integer.max - 1"
  )
})

test_that("MGM moderation values preserve signs and flag undefined direction", {
  negative <- quickNet:::quicknet_nira_mgm_interaction_values(
    list(
      weightsAgg = list(numeric(), list(1.75)),
      signs = list(numeric(), -1)
    ),
    expected_count = 1L
  )
  expect_equal(negative$magnitudes, 1.75)
  expect_equal(negative$signed_estimates, -1.75)
  expect_identical(negative$sign_defined, TRUE)

  undefined <- quickNet:::quicknet_nira_mgm_interaction_values(
    list(
      weightsAgg = list(numeric(), list(0.8)),
      signs = list(numeric(), 0)
    ),
    expected_count = 1L
  )
  expect_equal(undefined$magnitudes, 0.8)
  expect_true(is.na(undefined$signed_estimates))
  expect_identical(undefined$sign_defined, FALSE)
})

test_that("real MGM negative moderation is not reported as positive", {
  set.seed(18)
  n <- 250L
  first <- stats::rbinom(n, 1, 0.5)
  second <- stats::rbinom(n, 1, 0.5)
  third <- stats::rbinom(
    n,
    1,
    stats::plogis(2 - 4 * first * second)
  )
  data <- cbind(A = first, B = second, C = third)
  result <- quickNet:::quicknet_nira_run_moderation(
    data = data,
    node_names = colnames(data),
    rule = "AND",
    lambda = 0.25,
    nboot = 1L,
    stage_stream =
      quickNet:::quicknet_nira_make_streams(123L, 1L)[[1L]],
    use_parallel = FALSE,
    ncores = 1L
  )

  expect_true(all(result$table$estimate_scale == "signed"))
  expect_true(all(result$table$full_sample_estimate < 0))
  expect_true(all(result$table$mean_moderation_estimate < 0))
  expect_true(all(result$table$positive_proportion == 0))
  expect_true(all(result$table$negative_proportion == 1))
  expect_true(all(result$table$reference_sign == -1L))
})

test_that("PSOCK workers are thread-limited without changing the parent", {
  skip_if(parallel::detectCores() < 2L)
  thread_variables <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS"
  )
  before <- Sys.getenv(thread_variables, unset = NA_character_)
  cluster <- quickNet:::quicknet_nira_make_psock_cluster(2L)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  after <- Sys.getenv(thread_variables, unset = NA_character_)
  worker_values <- parallel::clusterCall(
    cluster,
    function(variables) Sys.getenv(variables),
    variables = thread_variables
  )

  expect_identical(after, before)
  expect_true(all(vapply(
    worker_values,
    function(values) all(values == "1"),
    logical(1)
  )))
  expect_lte(quickNet:::quicknet_nira_resolve_ncores(TRUE, NULL), 4L)
})

test_that("alleviating and aggravating change only the target threshold in the correct direction", {
  fit <- make_nira_test_fit()
  alleviating <- run_small_nira(
    fit,
    perturbation_type = "alleviating",
    amount_of_SDs_perturbation = 2
  )
  aggravating <- run_small_nira(
    fit,
    perturbation_type = "aggravating",
    amount_of_SDs_perturbation = 2
  )
  delta <- 2 * stats::sd(fit$nodes$threshold)
  expect_equal(alleviating$interventions$threshold_change, rep(-delta, 4))
  expect_equal(aggravating$interventions$threshold_change, rep(delta, 4))
  expect_equal(
    alleviating$interventions$intervened_threshold,
    fit$nodes$threshold - delta
  )
  expect_equal(
    aggravating$interventions$intervened_threshold,
    fit$nodes$threshold + delta
  )
})

test_that("fixed seeds reproduce primary simulations and tests", {
  fit <- make_nira_test_fit()
  one <- suppressWarnings(NIRA(
    fit,
    n_samples = 50,
    run_moderation = FALSE,
    n_permutations = 19,
    stability_reps = 3,
    top_n = 4,
    seed = 55,
    engine = "native"
  ))
  two <- suppressWarnings(NIRA(
    fit,
    n_samples = 50,
    run_moderation = FALSE,
    n_permutations = 19,
    stability_reps = 3,
    top_n = 4,
    seed = 55,
    engine = "native"
  ))
  expect_identical(one$baseline, two$baseline)
  expect_identical(one$interventions, two$interventions)
  expect_identical(one$permutation, two$permutation)
  expect_identical(one$stability, two$stability)
})

test_that("serial and PSOCK stability use the same task substreams", {
  skip_if(parallel::detectCores() < 2)
  fit <- make_nira_test_fit()
  serial <- suppressWarnings(NIRA(
    fit,
    n_samples = 30,
    run_moderation = FALSE,
    run_permutation = FALSE,
    stability_reps = 3,
    parallel = FALSE,
    seed = 56,
    engine = "native"
  ))
  parallel_result <- suppressWarnings(NIRA(
    fit,
    n_samples = 30,
    run_moderation = FALSE,
    run_permutation = FALSE,
    stability_reps = 3,
    parallel = TRUE,
    ncores = 2,
    seed = 56,
    engine = "native"
  ))
  expect_identical(serial$stability, parallel_result$stability)
})

test_that("moderation gate continues when no stable moderation is detected", {
  local_mocked_bindings(
    quicknet_nira_run_moderation = function(...) {
      list(
        table = data.frame(),
        stable_detected = FALSE,
        valid_reps = 2L,
        failed_reps = 0L,
        failure_messages = character(),
        rule = "AND",
        lambda = 0.25,
        nboot = 2L
      )
    },
    .package = "quickNet"
  )
  result <- suppressWarnings(NIRA(
    make_nira_test_fit(),
    n_samples = 30,
    moderation_nboot = 2,
    run_permutation = FALSE,
    run_stability = FALSE,
    engine = "native"
  ))
  expect_identical(result$status, "assumption_check_passed")
  expect_true(is.data.frame(result$interventions))
})

test_that("stable moderation blocks simulation by default", {
  local_mocked_bindings(
    quicknet_nira_run_moderation = function(...) {
      list(
        table = data.frame(
          moderator = "A",
          moderated_node_1 = "B",
          moderated_node_2 = "C",
          mean_moderation_estimate = 0.4,
          ci_lower = 0.2,
          ci_upper = 0.6,
          stable_moderation = TRUE
        ),
        stable_detected = TRUE,
        valid_reps = 2L,
        failed_reps = 0L
      )
    },
    .package = "quickNet"
  )
  result <- suppressWarnings(NIRA(
    make_nira_test_fit(),
    n_samples = 30,
    moderation_nboot = 2,
    run_permutation = FALSE,
    run_stability = FALSE,
    engine = "native"
  ))
  expect_identical(result$status, "blocked_by_moderation")
  expect_null(result$baseline)
  expect_null(result$interventions)
  expect_null(result$permutation)
  expect_null(result$stability)
})

test_that("explicit continuation preserves fixed-edge violation warnings", {
  local_mocked_bindings(
    quicknet_nira_run_moderation = function(...) {
      list(
        table = data.frame(
          moderator = "A",
          moderated_node_1 = "B",
          moderated_node_2 = "C",
          mean_moderation_estimate = 0.4,
          ci_lower = 0.2,
          ci_upper = 0.6,
          stable_moderation = TRUE
        ),
        stable_detected = TRUE,
        valid_reps = 2L,
        failed_reps = 0L
      )
    },
    .package = "quickNet"
  )
  warning_messages <- character()
  result <- withCallingHandlers(
    NIRA(
      make_nira_test_fit(),
      n_samples = 30,
      moderation_nboot = 2,
      proceed_on_moderation = TRUE,
      run_permutation = FALSE,
      run_stability = FALSE,
      engine = "native"
    ),
    warning = function(warning) {
      warning_messages <<- c(warning_messages, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("fixed-edge assumption is violated", warning_messages)))
  expect_identical(result$status, "completed_fixed_edge_assumption_violated")
  expect_true(any(grepl("fixed-edge", result$warnings)))
  expect_true(is.data.frame(result$interventions))
  expect_match(quicknet_report(result)$text, "fixed-edge assumption is violated")
  expect_match(
    plot(result, type = "effect")$labels$caption,
    "fixed-edge assumption is violated"
  )
})

test_that("permutation tests use the plus-one convention", {
  stream <- quickNet:::quicknet_nira_make_streams(90, 1)[[1L]]
  result <- quickNet:::quicknet_nira_permutation_one(
    original_scores = c(0, 0, 1, 1),
    intervention_scores = c(2, 2, 3, 3),
    n_permutations = 29,
    stream = stream
  )
  expect_equal(
    result$p_value,
    (result$extreme_count + 1) / (result$n_permutations + 1),
    tolerance = 0
  )
  expect_gte(result$p_value, 1 / 30)
})

test_that("all documented p adjustment methods are supported", {
  methods <- c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none")
  fit <- make_nira_test_fit()
  for (method in methods) {
    result <- suppressWarnings(NIRA(
      fit,
      n_samples = 20,
      run_moderation = FALSE,
      run_permutation = TRUE,
      n_permutations = 3,
      p_adjust = method,
      run_stability = FALSE,
      engine = "native"
    ))
    expect_equal(
      result$permutation$p_adjusted,
      stats::p.adjust(result$permutation$p_value, method = method)
    )
  }
})

test_that("stability rank counts and proportions satisfy full-rank invariants", {
  result <- suppressWarnings(NIRA(
    make_nira_test_fit(),
    n_samples = 30,
    run_moderation = FALSE,
    run_permutation = FALSE,
    stability_reps = 5,
    top_n = 4,
    engine = "native"
  ))
  frequencies <- result$stability$rank_frequencies
  expect_equal(
    as.vector(tapply(frequencies$count, frequencies$node, sum)),
    rep(result$stability$valid_reps, 4)
  )
  expect_equal(
    as.vector(tapply(frequencies$proportion, frequencies$node, sum)),
    rep(1, 4)
  )
  expect_equal(result$stability$valid_reps, 5)
  expect_equal(result$stability$failed_reps, 0)
  expect_true(all(result$stability$node_summary$top_k_cumulative_proportion == 1))
})

test_that("stability failures retain their repetition indices and errors", {
  local_mocked_bindings(
    quicknet_nira_stability_worker = function(task) {
      if (task$task_index == 2L) {
        return(list(
          ok = FALSE,
          task_index = task$task_index,
          condition_means = NULL,
          absolute_differences = NULL,
          ranks = NULL,
          error = "synthetic stability failure"
        ))
      }
      p <- length(task$thresholds)
      list(
        ok = TRUE,
        task_index = task$task_index,
        condition_means = seq_len(p + 1L),
        absolute_differences = rev(seq_len(p)),
        ranks = seq_len(p),
        error = NA_character_
      )
    },
    .package = "quickNet"
  )
  parameters <- quickNet:::quicknet_nira_extract_parameters(
    make_nira_test_fit()
  )
  stability <- suppressWarnings(
    quickNet:::quicknet_nira_run_stability(
      parameters = parameters,
      perturbation_type = "alleviating",
      threshold_delta = 1,
      n_samples = 2L,
      stability_reps = 5L,
      top_n = 4L,
      engine = "native",
      stage_stream =
        quickNet:::quicknet_nira_make_streams(91L, 1L)[[1L]],
      use_parallel = FALSE,
      ncores = 1L,
      n_iter = 1L
    )
  )
  expect_identical(stability$failed_rep_indices, 2L)
  expect_identical(stability$failure_records$repetition, 2L)
  expect_identical(
    stability$failure_records$error,
    "synthetic stability failure"
  )
  expect_true(all(is.na(stability$condition_means[2L, ])))
  expect_false("rep_2" %in% rownames(stability$rank_matrix))
})

test_that("quicknet_nira print summary plot and report methods work", {
  result <- suppressWarnings(NIRA(
    make_nira_test_fit(),
    n_samples = 30,
    run_moderation = FALSE,
    n_permutations = 5,
    stability_reps = 3,
    engine = "native"
  ))
  expect_output(print(result), "<quicknet_nira>")
  expect_type(summary(result), "list")
  expect_s3_class(plot(result, type = "effect"), "ggplot")
  expect_s3_class(plot(result, type = "rank"), "ggplot")
  expect_s3_class(plot(result, type = "stability"), "ggplot")
  expect_s3_class(plot(result, type = "moderation"), "ggplot")
  report <- quicknet_report(result)
  expect_s3_class(report, "quicknet_report")
  expect_match(report$text, "not causal", ignore.case = TRUE)
})

test_that("store_samples controls full binary sample retention", {
  omitted <- run_small_nira(store_samples = FALSE)
  retained <- run_small_nira(store_samples = TRUE)
  expect_null(omitted$samples)
  expect_equal(dim(retained$samples$original), c(40, 4))
  expect_length(retained$samples$interventions, 4)
})

test_that("legacy Ising threshold perturbation remains the default", {
  fit <- make_nira_test_fit()
  result <- Perturbation(
    fit,
    n_samples = 20,
    burnin = 10,
    thinning = 1,
    seed = 4
  )
  expect_s3_class(result, "quicknet_perturbation")
  expect_identical(result$method, "ising_threshold")
})

test_that("Perturbation method nira forwards to NIRA", {
  fit <- make_nira_test_fit()
  result <- suppressWarnings(Perturbation(
    fit,
    method = "nira",
    n_samples = 20,
    run_moderation = FALSE,
    run_permutation = FALSE,
    run_stability = FALSE,
    seed = 4,
    engine = "native",
    engine_iterations = 9L
  ))
  expect_s3_class(result, "quicknet_nira")
  expect_identical(result$settings$n_samples, 20L)
  expect_identical(result$settings$seed, 4L)
  expect_identical(result$settings$engine_iterations, 9L)
})

test_that("NIRA input registry and checker are synchronized", {
  fit <- make_nira_test_fit()
  expect_true("nira" %in% input_requirements()$model)
  expect_true("nira" %in% model_registry()$model)
  expect_true(check_input(model = "nira", fit = fit, quiet = TRUE)$ok)
})
