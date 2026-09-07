#' Network intervention ranking analysis for a fitted Ising network
#'
#' `NIRA()` implements the single-network simulation-intervention workflow
#' described by Wang et al. (2026). It first checks whether stable moderated
#' interactions contradict the fixed-edge assumption, then simulates an
#' original condition and one threshold intervention per node, performs
#' optional two-sample permutation tests, and evaluates Monte Carlo ranking
#' stability.
#'
#' The simulated effects are conditional on the fitted Ising parameters. They
#' are model-implied projections, not causal treatment effects, and they do not
#' propagate uncertainty from the original network estimation.
#'
#' @details
#' The analysis assumes cross-sectional binary variables, independent
#' observations, Ising local dependence with pairwise interactions, a
#' threshold interpretation that is meaningful for the selected node set, a
#' fixed edge-weight matrix after intervention, and a coherent construct with
#' a meaningful total score. These substantive assumptions cannot be inferred
#' automatically from a fitted object.
#'
#' Permutation p values compare independently generated score distributions
#' while holding the estimated network fixed; they do not include original
#' network-estimation uncertainty and are not evidence about real treatment
#' effects. Normal confidence intervals and pooled-SD Cohen's d retain their
#' usual distribution and variance assumptions. Confidence-interval overlap
#' is not used as a significance test. Monte Carlo rank stability is not
#' bootstrap network stability, empirical replicability, or evidence that one
#' rank differs significantly from another.
#' Exact ties in each stability repetition are randomly ordered using that
#' task's reproducible RNG stream, avoiding preference for earlier columns.
#'
#' @param fit A `quicknet_fit` object with `model = "ising"` or a
#'   confirmatory Ising model from which the binary data, thresholds, weight
#'   matrix, and inverse-temperature parameter can be extracted reliably.
#' @param perturbation_type Either `"alleviating"` (subtract the threshold
#'   delta) or `"aggravating"` (add the threshold delta).
#' @param amount_of_SDs_perturbation Positive number of threshold standard
#'   deviations used for each single-node intervention.
#' @param n_samples Number of independently initialized Ising chains simulated
#'   for every condition.
#' @param run_moderation Whether to test the fixed-edge assumption with a
#'   moderated graphical model before running NIRA.
#' @param moderation_rule MGM regularization rule, `"AND"` or `"OR"`. The
#'   default is inferred from `fit$meta$AND`; if unavailable, `"AND"` is used
#'   and recorded.
#' @param moderation_lambda EBIC tuning parameter supplied as `lambdaGam` to
#'   `mgm::mgm()`.
#' @param moderation_nboot Number of case-resampling repetitions for the
#'   moderation prerequisite. The default is exploratory; at least 1000 is
#'   recommended for formal work.
#' @param proceed_on_moderation If `FALSE`, stable moderation blocks the fixed
#'   edge simulation. If `TRUE`, simulation continues with an explicit
#'   assumption-violation warning.
#' @param run_permutation Whether to run independent two-sample label
#'   permutation tests comparing every intervention with the original
#'   condition.
#' @param n_permutations Number of label permutations per node.
#' @param p_adjust Multiple-testing method accepted by `stats::p.adjust()`.
#' @param run_stability Whether to repeat the complete set of condition
#'   simulations to estimate Monte Carlo ranking stability.
#' @param stability_reps Number of Monte Carlo stability repetitions. The
#'   default is suitable for formal analysis; 100 is intended only for
#'   development or exploration.
#' @param top_n Number of leading ranks used for the cumulative stability
#'   summary. `NULL` uses all nodes.
#' @param parallel Whether moderation resampling and stability repetitions may
#'   use a cross-platform PSOCK cluster.
#' @param ncores Positive number of workers. `NULL` selects a conservative
#'   value from `parallel::detectCores()`.
#' @param seed Non-negative integer seed. Task-specific L'Ecuyer-CMRG streams
#'   are used so task results do not depend on worker scheduling.
#' @param store_samples Whether to retain full binary sample matrices from the
#'   primary original and intervention conditions. Stability repetitions never
#'   retain binary matrices.
#' @param engine `"literature"` calls `IsingSampler::IsingSampler()` using its
#'   0/1 Metropolis-Hastings parameterization. `"native"` uses independently
#'   initialized vectorized Gibbs chains with the same conditional
#'   probabilities.
#' @param engine_iterations Positive number of Metropolis-Hastings or Gibbs
#'   sweeps used to initialize each independently simulated condition. The
#'   default of 100 matches the literature reference workflow. This is a
#'   sensitivity control, not an automatic convergence diagnostic.
#'
#' @return An object of class `quicknet_nira`.
#'
#' @references
#' Lunansky, G., Naberman, J., van Borkulo, C. D., Chen, C., Wang, L., and
#' Borsboom, D. (2022). Intervening on psychopathology networks: Evaluating
#' intervention targets through simulations. *Methods*, 204, 29-37.
#'
#' Wang, F., Wu, Y., Wu, Y., and Zhu, T. (2026). Simulation intervention for
#' cross-sectional network models: Based on the R packages nodeIdentifyR and
#' NIRApost. *Advances in Methods and Practices in Psychological Science*.
#' \doi{10.1177/25152459261452944}
#'
#' @examples
#' \dontrun{
#' fit <- quickNet(binary_data, model = "ising", gamma = 0.25,
#'                 AND = TRUE, pie = FALSE)
#' result <- NIRA(
#'   fit,
#'   perturbation_type = "alleviating",
#'   moderation_nboot = 1000,
#'   n_permutations = 5000,
#'   stability_reps = 1000,
#'   parallel = TRUE,
#'   ncores = 4,
#'   seed = 2025
#' )
#' summary(result)
#' result$rankings
#' quicknet_report(result)
#' plot(result, type = "effect")
#' plot(result, type = "stability")
#' }
#' @export
NIRA <- function(fit,
                 perturbation_type = c("alleviating", "aggravating"),
                 amount_of_SDs_perturbation = 2,
                 n_samples = 5000L,
                 run_moderation = TRUE,
                 moderation_rule = NULL,
                 moderation_lambda = 0.25,
                 moderation_nboot = 100L,
                 proceed_on_moderation = FALSE,
                 run_permutation = TRUE,
                 n_permutations = 5000L,
                 p_adjust = "bonferroni",
                 run_stability = TRUE,
                 stability_reps = 1000L,
                 top_n = NULL,
                 parallel = FALSE,
                 ncores = NULL,
                 seed = 2025L,
                 store_samples = FALSE,
                 engine = c("literature", "native"),
                 engine_iterations = 100L) {
  call <- match.call()
  perturbation_type <- match.arg(perturbation_type)
  engine <- match.arg(engine)
  quicknet_nira_validate_arguments(
    fit = fit,
    amount_of_SDs_perturbation = amount_of_SDs_perturbation,
    n_samples = n_samples,
    run_moderation = run_moderation,
    moderation_rule = moderation_rule,
    moderation_lambda = moderation_lambda,
    moderation_nboot = moderation_nboot,
    proceed_on_moderation = proceed_on_moderation,
    run_permutation = run_permutation,
    n_permutations = n_permutations,
    p_adjust = p_adjust,
    run_stability = run_stability,
    stability_reps = stability_reps,
    top_n = top_n,
    parallel = parallel,
    ncores = ncores,
    seed = seed,
    store_samples = store_samples,
    engine = engine,
    engine_iterations = engine_iterations
  )
  parameters <- quicknet_nira_extract_parameters(fit)
  threshold_perturbation <- quicknet_nira_threshold_perturbation(
    thresholds = parameters$thresholds,
    perturbation_type = perturbation_type,
    amount_of_SDs_perturbation = amount_of_SDs_perturbation
  )
  node_names <- names(parameters$thresholds)
  top_n <- if (is.null(top_n)) length(node_names) else as.integer(top_n)
  if (top_n > length(node_names)) {
    stop("top_n cannot exceed the number of nodes in fit.", call. = FALSE)
  }

  moderation_rule_inferred <- is.null(moderation_rule)
  if (moderation_rule_inferred) {
    moderation_rule <- if (identical(fit$meta$AND, FALSE)) "OR" else "AND"
  } else {
    moderation_rule <- toupper(moderation_rule)
  }

  ncores <- quicknet_nira_resolve_ncores(parallel, ncores)
  stage_streams <- quicknet_nira_make_streams(seed, 4L)
  rng_kind <- c("L'Ecuyer-CMRG", RNGkind()[2:3])
  analysis_warnings <- character()
  if (moderation_rule_inferred && is.null(fit$meta$AND) && run_moderation) {
    analysis_warnings <- c(
      analysis_warnings,
      "fit$meta$AND was unavailable; moderation_rule = 'AND' was used conservatively."
    )
  }
  construct_warning <- paste(
    "NIRA assumes that all nodes belong to a coherent construct and that their",
    "sum score is theoretically meaningful; this must be assessed by the user."
  )
  analysis_warnings <- c(analysis_warnings, construct_warning)
  warning(construct_warning, call. = FALSE)
  mixing_warning <- paste(
    "Finite-iteration MH/Gibbs sampling has no automatic convergence",
    "diagnostic; strong or multimodal networks may require sensitivity",
    "analyses with larger engine_iterations."
  )
  analysis_warnings <- c(analysis_warnings, mixing_warning)

  settings <- list(
    perturbation_type = perturbation_type,
    amount_of_SDs_perturbation = amount_of_SDs_perturbation,
    threshold_delta = threshold_perturbation$threshold_delta,
    n_samples = as.integer(n_samples),
    run_moderation = run_moderation,
    moderation_rule = moderation_rule,
    moderation_rule_inferred = moderation_rule_inferred,
    moderation_lambda = moderation_lambda,
    moderation_nboot = as.integer(moderation_nboot),
    proceed_on_moderation = proceed_on_moderation,
    run_permutation = run_permutation,
    n_permutations = as.integer(n_permutations),
    p_adjust = p_adjust,
    run_stability = run_stability,
    stability_reps = as.integer(stability_reps),
    top_n = top_n,
    parallel = parallel,
    ncores = ncores,
    seed = as.integer(seed),
    store_samples = store_samples,
    engine = engine,
    engine_iterations = as.integer(engine_iterations)
  )
  provenance <- quicknet_nira_provenance(
    fit = fit,
    settings = settings,
    rng_kind = rng_kind,
    stage_streams = stage_streams,
    beta = parameters$beta
  )

  moderation <- NULL
  stable_moderation <- FALSE
  status <- "completed_moderation_not_tested"
  if (run_moderation) {
    moderation <- quicknet_nira_run_moderation(
      data = parameters$data,
      node_names = node_names,
      rule = moderation_rule,
      lambda = moderation_lambda,
      nboot = as.integer(moderation_nboot),
      stage_stream = stage_streams[[1L]],
      use_parallel = parallel,
      ncores = ncores
    )
    stable_moderation <- isTRUE(moderation$stable_detected)
    status <- if (stable_moderation) {
      if (proceed_on_moderation) {
        "completed_fixed_edge_assumption_violated"
      } else {
        "blocked_by_moderation"
      }
    } else {
      "assumption_check_passed"
    }
  } else {
    analysis_warnings <- c(
      analysis_warnings,
      "Moderation was not tested; edge-weight invariance has not been established."
    )
  }

  if (stable_moderation) {
    fixed_edge_warning <- paste(
      "Stable moderation was detected. The fixed-edge assumption is violated;",
      if (proceed_on_moderation) {
        "simulation is continuing only because proceed_on_moderation = TRUE."
      } else {
        "NIRA simulation was blocked."
      }
    )
    analysis_warnings <- c(analysis_warnings, fixed_edge_warning)
    if (proceed_on_moderation) {
      warning(fixed_edge_warning, call. = FALSE)
    }
  }

  assumptions <- quicknet_nira_assumptions(
    run_moderation = run_moderation,
    stable_moderation = stable_moderation,
    proceed_on_moderation = proceed_on_moderation
  )
  if (identical(status, "blocked_by_moderation")) {
    return(quicknet_nira_object(
      call = call,
      model = fit$model,
      status = status,
      settings = settings,
      provenance = provenance,
      assumptions = assumptions,
      moderation = moderation,
      baseline = NULL,
      interventions = NULL,
      permutation = NULL,
      stability = NULL,
      rankings = NULL,
      samples = NULL,
      warnings = unique(analysis_warnings)
    ))
  }

  if (run_permutation) {
    analysis_warnings <- c(
      analysis_warnings,
      paste(
        "Permutation p-values condition on the fixed estimated network and",
        "do not include original network-estimation uncertainty."
      ),
      paste(
        "Normal confidence intervals and pooled-SD Cohen's d retain",
        "distribution and variance assumptions; CI overlap is not used",
        "as a significance test."
      )
    )
  }
  if (run_stability) {
    analysis_warnings <- c(
      analysis_warnings,
      paste(
        "Monte Carlo rank stability is not bootstrap network stability or",
        "evidence that adjacent ranks differ significantly."
      )
    )
  }

  conditions <- quicknet_nira_run_conditions(
    parameters = parameters,
    perturbation_type = perturbation_type,
    amount_of_SDs_perturbation = amount_of_SDs_perturbation,
    n_samples = as.integer(n_samples),
    engine = engine,
    stage_stream = stage_streams[[2L]],
    store_samples = store_samples,
    n_iter = as.integer(engine_iterations)
  )
  settings$threshold_delta <- conditions$threshold_delta

  permutation <- if (run_permutation) {
    quicknet_nira_run_permutation(
      original_scores = conditions$baseline$total_scores,
      intervention_scores = conditions$intervention_scores,
      intervention_statistics = conditions$interventions,
      perturbation_type = perturbation_type,
      n_permutations = as.integer(n_permutations),
      p_adjust = p_adjust,
      stage_stream = stage_streams[[3L]]
    )
  } else {
    NULL
  }

  stability <- if (run_stability) {
    quicknet_nira_run_stability(
      parameters = parameters,
      perturbation_type = perturbation_type,
      threshold_delta = conditions$threshold_delta,
      n_samples = as.integer(n_samples),
      stability_reps = as.integer(stability_reps),
      top_n = top_n,
      engine = engine,
      stage_stream = stage_streams[[4L]],
      use_parallel = parallel,
      ncores = ncores,
      n_iter = as.integer(engine_iterations)
    )
  } else {
    NULL
  }

  rankings <- quicknet_nira_rankings(
    fit = fit,
    interventions = conditions$interventions,
    stability = stability
  )
  quicknet_nira_object(
    call = call,
    model = fit$model,
    status = status,
    settings = settings,
    provenance = provenance,
    assumptions = assumptions,
    moderation = moderation,
    baseline = conditions$baseline,
    interventions = conditions$interventions,
    permutation = permutation,
    stability = stability,
    rankings = rankings,
    samples = conditions$samples,
    warnings = unique(analysis_warnings)
  )
}

quicknet_nira_is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    abs(x - round(x)) <= sqrt(.Machine$double.eps)
}

quicknet_nira_is_integer_count <- function(x, minimum = 1L) {
  quicknet_nira_is_whole_number(x) &&
    x >= minimum &&
    x <= .Machine$integer.max
}

quicknet_nira_is_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

quicknet_nira_validate_arguments <- function(fit,
                                             amount_of_SDs_perturbation,
                                             n_samples,
                                             run_moderation,
                                             moderation_rule,
                                             moderation_lambda,
                                             moderation_nboot,
                                             proceed_on_moderation,
                                             run_permutation,
                                             n_permutations,
                                             p_adjust,
                                             run_stability,
                                             stability_reps,
                                             top_n,
                                             parallel,
                                             ncores,
                                             seed,
                                             store_samples,
                                             engine,
                                             engine_iterations) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  if (!quicknet_is_ising_model(fit$model)) {
    stop("NIRA supports only exploratory or confirmatory Ising quicknet_fit objects.", call. = FALSE)
  }
  if (!is.numeric(amount_of_SDs_perturbation) ||
      length(amount_of_SDs_perturbation) != 1L ||
      !is.finite(amount_of_SDs_perturbation) ||
      amount_of_SDs_perturbation <= 0) {
    stop("amount_of_SDs_perturbation must be one positive finite number.", call. = FALSE)
  }
  if (!quicknet_nira_is_integer_count(n_samples, minimum = 2L)) {
    stop(
      "n_samples must be an integer from 2 through .Machine$integer.max.",
      call. = FALSE
    )
  }
  flag_names <- c(
    "run_moderation", "proceed_on_moderation", "run_permutation",
    "run_stability", "parallel", "store_samples"
  )
  flags <- list(
    run_moderation, proceed_on_moderation, run_permutation,
    run_stability, parallel, store_samples
  )
  invalid_flag <- !vapply(flags, quicknet_nira_is_flag, logical(1))
  if (any(invalid_flag)) {
    stop(flag_names[which(invalid_flag)[[1L]]], " must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(moderation_rule) ||
      (is.character(moderation_rule) && length(moderation_rule) == 0L)) {
    if (!is.character(moderation_rule) || length(moderation_rule) != 1L ||
        is.na(moderation_rule) || !toupper(moderation_rule) %in% c("AND", "OR")) {
      stop("moderation_rule must be NULL, 'AND', or 'OR'.", call. = FALSE)
    }
  }
  if (!is.numeric(moderation_lambda) || length(moderation_lambda) != 1L ||
      !is.finite(moderation_lambda) || moderation_lambda < 0 ||
      moderation_lambda > 1) {
    stop("moderation_lambda must be one finite number between 0 and 1.", call. = FALSE)
  }
  if (!quicknet_nira_is_integer_count(moderation_nboot) ||
      moderation_nboot > .Machine$integer.max - 1) {
    stop(
      paste0(
        "moderation_nboot must be a positive integer no larger than ",
        ".Machine$integer.max - 1."
      ),
      call. = FALSE
    )
  }
  if (!quicknet_nira_is_integer_count(n_permutations)) {
    stop(
      "n_permutations must be a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  allowed_adjustments <- c(
    "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"
  )
  if (!is.character(p_adjust) || length(p_adjust) != 1L ||
      is.na(p_adjust) || !p_adjust %in% allowed_adjustments) {
    stop(
      "p_adjust must be one of: ",
      paste(allowed_adjustments, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!quicknet_nira_is_integer_count(stability_reps)) {
    stop(
      "stability_reps must be a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  if (!is.null(top_n) &&
      !quicknet_nira_is_integer_count(top_n)) {
    stop(
      "top_n must be NULL or a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  if (!is.null(ncores) &&
      !quicknet_nira_is_integer_count(ncores)) {
    stop(
      "ncores must be NULL or a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  if (!quicknet_nira_is_whole_number(seed) || seed < 0 ||
      seed > .Machine$integer.max) {
    stop(
      "seed must be a non-negative integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  if (identical(engine, "literature") &&
      !requireNamespace("IsingSampler", quietly = TRUE)) {
    stop(
      "engine = 'literature' requires the IsingSampler package.",
      call. = FALSE
    )
  }
  if (!quicknet_nira_is_integer_count(engine_iterations)) {
    stop(
      "engine_iterations must be a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

quicknet_nira_resolve_ncores <- function(use_parallel, ncores) {
  if (!is.null(ncores)) return(as.integer(ncores))
  if (!use_parallel) return(1L)
  detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(detected) != 1L || !is.finite(detected) || detected < 1L) {
    detected <- 1L
  }
  max(1L, min(4L, as.integer(detected) - 1L))
}

quicknet_nira_extract_parameters <- function(fit) {
  data <- fit$data
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("fit$data must contain the binary observations used to estimate the network.", call. = FALSE)
  }
  data <- as.data.frame(data, check.names = FALSE)
  if (ncol(data) < 2L) {
    stop("NIRA requires at least two nodes.", call. = FALSE)
  }
  data_names <- colnames(data)
  if (is.null(data_names) || anyNA(data_names) || any(data_names == "") ||
      anyDuplicated(data_names)) {
    stop("fit$data must have non-empty, unique node names.", call. = FALSE)
  }
  numeric_columns <- vapply(data, is.numeric, logical(1))
  if (!all(numeric_columns)) {
    stop("All NIRA analysis columns must be numeric and coded exactly 0/1.", call. = FALSE)
  }
  data_matrix <- as.matrix(data)
  storage.mode(data_matrix) <- "double"
  if (anyNA(data_matrix) || any(!is.finite(data_matrix))) {
    stop("NIRA does not impute missing or non-finite values in fit$data.", call. = FALSE)
  }
  if (any(!data_matrix %in% c(0, 1))) {
    stop("All NIRA analysis values must be coded exactly 0/1.", call. = FALSE)
  }

  weight_matrix <- fit$graph
  if (is.null(weight_matrix)) {
    stop("fit$graph is required for NIRA.", call. = FALSE)
  }
  weight_matrix <- as.matrix(weight_matrix)
  if (!is.numeric(weight_matrix) || length(dim(weight_matrix)) != 2L ||
      nrow(weight_matrix) != ncol(weight_matrix) ||
      nrow(weight_matrix) != ncol(data_matrix)) {
    stop("fit$graph must be a numeric square matrix matching fit$data.", call. = FALSE)
  }
  row_names <- rownames(weight_matrix)
  column_names <- colnames(weight_matrix)
  if (is.null(row_names) || is.null(column_names) ||
      anyNA(row_names) || anyNA(column_names) ||
      any(row_names == "") || any(column_names == "") ||
      anyDuplicated(row_names) || anyDuplicated(column_names)) {
    stop("fit$graph must have non-empty, unique row and column node names.", call. = FALSE)
  }
  if (!identical(row_names, column_names) ||
      !identical(row_names, data_names)) {
    stop(
      "Node names and order in fit$data and fit$graph must be exactly identical; NIRA does not silently reorder nodes.",
      call. = FALSE
    )
  }
  storage.mode(weight_matrix) <- "double"
  if (anyNA(weight_matrix) || any(!is.finite(weight_matrix))) {
    stop("fit$graph must contain only finite values.", call. = FALSE)
  }
  symmetry_tolerance <- 100 * .Machine$double.eps *
    max(1, max(abs(weight_matrix)))
  if (max(abs(weight_matrix - t(weight_matrix))) > symmetry_tolerance) {
    stop("fit$graph must be symmetric for an Ising NIRA analysis.", call. = FALSE)
  }
  if (any(abs(diag(weight_matrix)) > symmetry_tolerance)) {
    stop("fit$graph must have an exactly zero diagonal within numerical tolerance.", call. = FALSE)
  }

  nodes <- fit$nodes
  if (!is.data.frame(nodes) || !all(c("node", "threshold") %in% names(nodes))) {
    stop("fit$nodes must contain node and threshold columns.", call. = FALSE)
  }
  threshold_names <- as.character(nodes$node)
  if (length(threshold_names) != length(data_names) ||
      anyNA(threshold_names) || any(threshold_names == "") ||
      anyDuplicated(threshold_names)) {
    stop("fit$nodes must provide exactly one named threshold for every node.", call. = FALSE)
  }
  if (!identical(threshold_names, data_names)) {
    stop(
      "Threshold node names and order must exactly match fit$data and fit$graph; NIRA does not silently reorder nodes.",
      call. = FALSE
    )
  }
  thresholds <- nodes$threshold
  if (!is.numeric(thresholds) || length(thresholds) != length(data_names) ||
      anyNA(thresholds) || any(!is.finite(thresholds))) {
    stop("All Ising thresholds must be finite numeric values.", call. = FALSE)
  }
  thresholds <- stats::setNames(as.numeric(thresholds), threshold_names)
  threshold_sd <- stats::sd(thresholds)
  if (!is.finite(threshold_sd) || threshold_sd <= 0) {
    stop("sd(thresholds) must be greater than zero for NIRA.", call. = FALSE)
  }

  if (identical(fit$model, "ising") && !is.null(fit$fit$thresholds)) {
    backend_thresholds <- fit$fit$thresholds
    if (is.null(names(backend_thresholds)) ||
        !identical(names(backend_thresholds), data_names)) {
      stop(
        "fit$fit$thresholds is not named in exactly the same order as fit$data.",
        call. = FALSE
      )
    }
    if (length(backend_thresholds) != length(thresholds) ||
        anyNA(backend_thresholds) || any(!is.finite(backend_thresholds)) ||
        !isTRUE(all.equal(
          as.numeric(backend_thresholds),
          as.numeric(thresholds),
          tolerance = 1e-10,
          check.attributes = FALSE
        ))) {
      stop("fit$nodes thresholds disagree with fit$fit$thresholds.", call. = FALSE)
    }
  }

  beta <- if (identical(fit$model, "confirmatory_ising")) {
    quicknet_nira_extract_confirmatory_beta(fit)
  } else {
    1
  }
  list(
    data = data_matrix,
    weight_matrix = weight_matrix,
    thresholds = thresholds,
    beta = beta
  )
}

quicknet_nira_extract_confirmatory_beta <- function(fit) {
  model <- fit$fit$model
  if (is.null(model) || !requireNamespace("psychonetrics", quietly = TRUE)) {
    stop(
      "The confirmatory Ising inverse-temperature parameter could not be extracted reliably.",
      call. = FALSE
    )
  }
  beta <- tryCatch(
    as.numeric(psychonetrics::getmatrix(model, "beta")),
    error = function(error) numeric()
  )
  if (length(beta) != 1L || !is.finite(beta) || beta <= 0) {
    log_beta <- tryCatch(
      as.numeric(psychonetrics::getmatrix(model, "log_beta")),
      error = function(error) numeric()
    )
    beta <- if (length(log_beta) == 1L && is.finite(log_beta)) {
      exp(log_beta)
    } else {
      numeric()
    }
  }
  if (length(beta) != 1L || !is.finite(beta) || beta <= 0) {
    stop(
      "The confirmatory Ising beta parameter must be one positive finite value.",
      call. = FALSE
    )
  }
  beta
}

quicknet_nira_assumptions <- function(run_moderation,
                                      stable_moderation,
                                      proceed_on_moderation) {
  fixed_edge_status <- if (!run_moderation) {
    "not tested"
  } else if (stable_moderation) {
    if (proceed_on_moderation) "violated; analysis continued by explicit request" else "violated; analysis blocked"
  } else {
    "no stable moderation detected; this is not proof of absence"
  }
  data.frame(
    assumption = c(
      "cross-sectional binary variables",
      "independent observations",
      "Ising local dependence",
      "pairwise interactions",
      "thresholds represent spontaneous activation tendency",
      "node set is sufficient to define and interpret thresholds",
      "intervention leaves the edge-weight matrix fixed",
      "nodes form a coherent construct that can be summed",
      "total score has theoretical or clinical meaning",
      "finite-iteration simulation mixing is adequate",
      "simulation is not a causal treatment effect",
      "clinical use requires external intervention validation"
    ),
    status = c(
      "required and structurally checked",
      "required; user/data-design assessment",
      "model assumption",
      "model assumption",
      "interpretive assumption",
      "requires user assessment",
      fixed_edge_status,
      "requires user assessment",
      "requires user assessment",
      "not diagnosed automatically; assess sensitivity to engine_iterations",
      "explicit interpretation boundary",
      "required before clinical interpretation"
    ),
    stringsAsFactors = FALSE
  )
}

quicknet_nira_provenance <- function(fit,
                                     settings,
                                     rng_kind,
                                     stage_streams,
                                     beta) {
  package_version <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(package))
  }
  list(
    implementation = "quickNet independent implementation from published method descriptions",
    reference = "Wang et al. (2026), doi:10.1177/25152459261452944",
    source_license = "MIT",
    model = fit$model,
    R_version = R.version.string,
    platform = R.version$platform,
    package_versions = c(
      quickNet = package_version("quickNet"),
      IsingSampler = package_version("IsingSampler"),
      IsingFit = package_version("IsingFit"),
      mgm = package_version("mgm"),
      psychonetrics = package_version("psychonetrics")
    ),
    engine = settings$engine,
    engine_iterations = settings$engine_iterations,
    beta = beta,
    seed = settings$seed,
    RNGkind = rng_kind,
    task_stream_strategy = paste(
      "one L'Ecuyer-CMRG stream per stage and one deterministic",
      "substream per task"
    ),
    psock_worker_thread_policy = paste(
      "one BLAS/OpenMP thread per worker to prevent nested",
      "thread oversubscription"
    ),
    stage_streams = lapply(stage_streams, as.integer)
  )
}

quicknet_nira_rankings <- function(fit, interventions, stability) {
  nodes <- interventions$node
  out <- data.frame(
    node = nodes,
    directional_effect = interventions$directional_effect,
    directional_effect_rank = rank(
      -interventions$directional_effect,
      ties.method = "min",
      na.last = "keep"
    ),
    absolute_mean_difference = interventions$absolute_mean_difference,
    absolute_effect_rank = rank(
      -interventions$absolute_mean_difference,
      ties.method = "min",
      na.last = "keep"
    ),
    stringsAsFactors = FALSE
  )
  if (!is.null(stability)) {
    stability_order <- match(nodes, stability$node_summary$node)
    out$top1_stability_proportion <-
      stability$node_summary$top1_proportion[stability_order]
    out$top1_stability_rank <- rank(
      -out$top1_stability_proportion,
      ties.method = "min",
      na.last = "keep"
    )
  } else {
    out$top1_stability_proportion <- NA_real_
    out$top1_stability_rank <- NA_integer_
  }

  node_table <- fit$nodes
  node_order <- match(nodes, node_table$node)
  if ("strength" %in% names(node_table)) {
    out$strength <- node_table$strength[node_order]
    out$strength_rank <- rank(-out$strength, ties.method = "min", na.last = "keep")
  }
  if ("expected_influence" %in% names(node_table)) {
    out$expected_influence <- node_table$expected_influence[node_order]
    out$expected_influence_rank <- rank(
      -out$expected_influence,
      ties.method = "min",
      na.last = "keep"
    )
  }
  out[order(out$directional_effect_rank, out$absolute_effect_rank), , drop = FALSE]
}

quicknet_nira_object <- function(call,
                                 model,
                                 status,
                                 settings,
                                 provenance,
                                 assumptions,
                                 moderation,
                                 baseline,
                                 interventions,
                                 permutation,
                                 stability,
                                 rankings,
                                 samples,
                                 warnings) {
  structure(
    list(
      call = call,
      model = model,
      status = status,
      settings = settings,
      provenance = provenance,
      assumptions = assumptions,
      moderation = moderation,
      baseline = baseline,
      interventions = interventions,
      permutation = permutation,
      stability = stability,
      rankings = rankings,
      samples = samples,
      warnings = warnings
    ),
    class = "quicknet_nira"
  )
}

#' @export
print.quicknet_nira <- function(x, ...) {
  cat("<quicknet_nira>\n")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Status: ", x$status, "\n", sep = "")
  cat(
    "Engine: ", x$settings$engine,
    "; perturbation: ", x$settings$perturbation_type,
    "; threshold delta: ", format(x$settings$threshold_delta, digits = 5L),
    "\n",
    sep = ""
  )
  if (!is.null(x$interventions) && nrow(x$interventions) > 0L) {
    best <- x$interventions[
      which.max(x$interventions$directional_effect),
      ,
      drop = FALSE
    ]
    cat(
      "Largest projected directional effect: ",
      best$node[[1L]], " (",
      format(best$directional_effect[[1L]], digits = 5L), ")\n",
      sep = ""
    )
  }
  if (length(x$warnings) > 0L) {
    cat("Warnings/interpretation boundaries:\n")
    for (message in x$warnings) cat("- ", message, "\n", sep = "")
  }
  cat("Model-implied simulation; not a causal treatment effect.\n")
  invisible(x)
}

#' @export
summary.quicknet_nira <- function(object, ...) {
  list(
    status = object$status,
    settings = object$settings,
    assumptions = object$assumptions,
    moderation = object$moderation,
    effects = object$interventions,
    permutation = object$permutation,
    stability = if (is.null(object$stability)) NULL else object$stability$node_summary,
    rankings = object$rankings,
    warnings = object$warnings
  )
}
