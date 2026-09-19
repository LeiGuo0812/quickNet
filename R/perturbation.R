#' Virtual perturbation and intervention simulation
#'
#' @param fit A \code{quicknet_fit} object.
#' @param method Perturbation method. Continuous networks support
#' \code{"dosage"}, \code{"knockout"}, \code{"knockdown"},
#' \code{"edge_block"}, \code{"node_block"}, \code{"combination"},
#' \code{"sequence"}, and the complete seven-utility \code{"symperturb"} workflow.
#' Ising networks support \code{"ising_threshold"} and the formal
#' \code{"nira"} workflow. The existing \code{"ising_threshold"} method is
#' unchanged.
#' @param targets Candidate node names. If \code{NULL}, all nodes are considered.
#' State methods also accept a list of jointly intervened target sets.
#' @param dose State intervention fractions in [0,1]. Explicit values override
#' \code{config$dose_grid} for dosage and full analysis. Knockout, combination,
#' and sequence use unit dose. Knockdown accepts one or more fractions.
#' @param remaining_strength For knockdown, an alternative to \code{dose}:
#' retained target location/scale fraction, so alpha = 1 - remaining_strength.
#' This no longer attenuates precision-matrix edges. Do not supply both arguments.
#' @param edges Optional edge table for \code{method = "edge_block"}. It can
#' contain \code{from/to} or \code{node_i/node_j} columns. If \code{NULL}, all
#' nonzero topology edges incident to the candidate targets are considered.
#' @param combination_size Must be 2 for the reference pair increment.
#' @param steps Maximum sequence length for beam search. An explicitly supplied
#' value overrides \code{config$sequence_length}. The standalone sequence method
#' uses 4 when no sequence length is configured; full analysis defaults to 0.
#' @param threshold_shift Threshold shift for Ising perturbation. Negative
#' values lower target activation tendency.
#' @param n_samples Number of Gibbs samples for Ising perturbation.
#' @param burnin Number of burn-in sweeps for Ising perturbation.
#' @param thinning Thinning interval for Ising perturbation.
#' @param seed Random seed. Explicitly supplied values override
#' \code{config$random_seed} for continuous analysis; its reference default is
#' 20260727. Existing Ising defaults are preserved.
#' @param pulse_values,spillover_nodes Retired pulse-conditioning arguments.
#' Non-NULL values raise a migration error; communication blocking now uses
#' the finite-step adjacency propagation functional.
#' @param threshold Explicitly supplied values override
#' \code{config$edge_threshold}. Thresholding applies only to topology, never
#' to the state covariance.
#' @param perturbation_type NIRA direction, \code{"alleviating"} or
#' \code{"aggravating"}.
#' @param amount_of_SDs_perturbation NIRA threshold perturbation size in
#' threshold standard deviations.
#' @param run_moderation,moderation_rule,moderation_lambda,moderation_nboot
#' NIRA moderation-prerequisite settings; see \code{\link{NIRA}}.
#' @param proceed_on_moderation Whether NIRA may continue after stable
#' moderation is detected.
#' @param run_permutation,n_permutations,p_adjust NIRA permutation settings.
#' @param run_stability,stability_reps,top_n NIRA Monte Carlo stability
#' settings.
#' @param parallel,ncores Cross-platform NIRA parallel settings.
#' @param store_samples Whether NIRA retains full primary binary simulations.
#' @param engine NIRA simulation engine.
#' @param engine_iterations NIRA simulation sweeps per independently
#'   initialized condition; see \code{\link{NIRA}}.
#' @param config Named list of SymPerturb settings, using the Python reference
#' names and defaults; see Details. Unknown names are rejected.
#' @param modules Named vector assigning each symptom to a module. Full analysis
#' and sequence optimization require at least two modules (sequence pool ordering
#' uses the same scored candidate table as the full reference analysis).
#' A character/factor \code{groups} vector from
#' the original fit can also supply this mapping.
#' @param anchors Named numeric vector of target anchors; unspecified nodes use 0.
#' @param symptom_weights Named non-negative outcome weights; unspecified nodes
#' use 1. An outcome set with zero total weight uses its unweighted mean.
#' @param costs Named non-negative sequence costs; unspecified nodes use 0.
#' @param bootstrap_indices Optional integer matrix with
#' \code{config$bootstrap_replicates} rows and one column per participant.
#' Entries are one-based participant indices. Use this with full analysis to
#' reproduce precisely the same resamples across languages.
#'
#' @details
#' Continuous methods re-estimate a Gaussian network from the original finite
#' numeric participant data in \code{fit$data}, using sample covariance plus
#' ridge times its diagonal. The fitted quickNet graph is not the state model.
#' At least three participants and three symptoms are required. Missing values
#' must be handled explicitly before analysis. The state operator updates both
#' mean and covariance; exact linked knockout has zero target variance and never
#' re-estimates a network containing that constant target column.
#'
#' The supported \code{config} entries and defaults are:
#' \describe{
#' \item{Network}{\code{ridge = 0.02}, \code{edge_threshold = 0.03}.}
#' \item{State}{\code{bounds = c(0,4)} (use NULL for unbounded outcomes),
#' \code{state_map = "linked"} (also "location_only", "scale_only", "independent"),
#' \code{mu_power = 1}, \code{sigma_power = 1}. Independent maps use separate
#' positive powers of (1-alpha). Location-only and scale-only maps are sensitivity
#' analyses and do not impose the linked exact-knockout endpoint.}
#' \item{Doses}{\code{dose_grid = c(0,.10,.25,.50,.75,1)},
#' \code{dose_efficiency_grid = c(.25,.50,.75)},
#' \code{responsiveness_epsilon = .10}.}
#' \item{Utilities}{\code{breadth_threshold = .10}, \code{module_threshold = .20},
#' \code{combination_partner_k = 5}, \code{combination_mode = "signed"}
#' ("positive" enables historical exploratory positive-part averaging),
#' \code{vpps_weights = numeric()} (named utility weights; unspecified weights are 1).}
#' \item{Topology}{\code{block_fraction = .80}, \code{propagation_steps = 6},
#' \code{propagation_gamma = .45}, \code{propagation_absolute = TRUE},
#' \code{adjacency_normalization = "raw"} (also "row" or "spectral").}
#' \item{Uncertainty}{\code{run_robustness_scenarios = TRUE},
#' \code{bootstrap_replicates = 0}, \code{bootstrap_top_k = 5},
#' \code{random_seed = 20260727}. These are run by the full workflow only.
#' Bootstrap uses R's RNG; equal numeric seeds do not imply equal NumPy samples.
#' Use shared \code{bootstrap_indices} for cross-language numerical equality.}
#' \item{Sequence}{\code{sequence_length = 0}, \code{sequence_pool = 8},
#' \code{sequence_beam_width = 20}, \code{sequence_eta = .90},
#' \code{sequence_cost_lambda = 0}.}
#' }
#'
#' Seven utilities are efficacy, dose efficiency, breadth, cross-module reach,
#' communication block, combination value, and responsiveness. Each is scaled
#' within the candidate set to 0--100 (constant dimensions score 50); VPPS is
#' their weighted mean. Robustness is separate. The complete pipeline is refit
#' for every bootstrap replicate. Pair value compares the joint benefit against
#' the better single-target benefit on the common non-target set; it is not
#' additive synergy. Sequence search optimizes discounted marginal downstream
#' benefit minus cost, and does not identify biological time ordering.
#'
#' @return A \code{quicknet_perturbation} object, or a
#' \code{quicknet_nira} object for \code{method = "nira"}. Results are
#' model-implied in silico simulations and should not be interpreted as causal
#' intervention effects. Continuous results include \code{network},
#' \code{network_edges}, \code{metadata}, \code{baseline}, \code{metrics},
#' \code{rankings}, and state \code{moments}/\code{perturbations} when applicable.
#' State rankings use \code{system_benefit} (weighted standardized non-target
#' improvement); raw \code{burden_reduction} is descriptive only.
#' Combination metrics use \code{incremental_pair_value}, replacing \code{synergy}.
#' Topology metrics use \code{communication_block}, replacing pulse spillover.
#' Sequence results retain all final beam candidates plus \code{sequence_paths}.
#' Full analysis additionally includes \code{target_scores}, \code{dose_response},
#' \code{pair_scores}, \code{robustness}, \code{scenario_ranks}, \code{bootstrap},
#' \code{bootstrap_draws}, \code{bootstrap_indices}, and \code{sequence}.
#' \code{summary()} returns the primary metrics table; \code{quicknet_report()}
#' retains the full analysis tables.
#' @references Zhu, Z., Yu, J., Hu, T., Yang, Z., and Wang, J. (2026).
#' SymPerturb converts symptom-network structure into testable intervention
#' priorities. arXiv:2607.28673v1. Revised method specification and SymPerturb 0.1.0.
#' @examples
#' fit <- quickNet(mtcars[, 1:5], model = "correlation", pie = FALSE,
#'                 DoNotPlot = TRUE)
#' Perturbation(fit, "knockout", targets = "mpg", config = list(bounds = NULL))
#' modules <- c(mpg = "performance", cyl = "engine", disp = "engine",
#'              hp = "engine", drat = "performance")
#' result <- Perturbation(fit, "symperturb", modules = modules,
#'                       config = list(bounds = NULL, run_robustness_scenarios = FALSE))
#' summary(result)
#' @export
Perturbation <- function(fit,
                         method = c("dosage", "knockout", "knockdown", "edge_block", "combination", "sequence", "ising_threshold", "nira", "node_block", "symperturb"),
                         targets = NULL,
                         dose = c(0, 0.10, 0.25, 0.50, 0.75, 1.00),
                         remaining_strength = 0.50,
                         edges = NULL,
                         combination_size = 2,
                         steps = 4,
                         threshold_shift = -1,
                         n_samples = 1200,
                         burnin = 600,
                         thinning = 2,
                         seed = 20260502,
                         pulse_values = NULL,
                         spillover_nodes = NULL,
                         threshold = 0.03,
                         perturbation_type = c("alleviating", "aggravating"),
                         amount_of_SDs_perturbation = 2,
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
                         store_samples = FALSE,
                         engine = c("literature", "native"),
                         engine_iterations = 100L,
                         config = list(),
                         modules = NULL,
                         anchors = NULL,
                         symptom_weights = NULL,
                         costs = NULL,
                         bootstrap_indices = NULL) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  n_samples_missing <- missing(n_samples)
  seed_missing <- missing(seed)
  method <- if (missing(method)) {
    if (quicknet_is_ising_model(fit$model)) "ising_threshold" else "dosage"
  } else {
    match.arg(method)
  }
  quicknet_validate_input(fit = fit, model = "perturbation", method = method)

  if (method == "ising_threshold") {
    return(quicknet_perturb_ising(
      fit = fit,
      targets = targets,
      threshold_shift = threshold_shift,
      n_samples = n_samples,
      burnin = burnin,
      thinning = thinning,
      seed = seed
    ))
  }

  if (method == "nira") {
    if (!is.null(targets)) {
      stop(
        "method = 'nira' evaluates every node; targets must be NULL.",
        call. = FALSE
      )
    }
    return(NIRA(
      fit = fit,
      perturbation_type = perturbation_type,
      amount_of_SDs_perturbation = amount_of_SDs_perturbation,
      n_samples = if (n_samples_missing) 5000L else n_samples,
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
      seed = if (seed_missing) 2025L else seed,
      store_samples = store_samples,
      engine = engine,
      engine_iterations = engine_iterations
    ))
  }

  quicknet_perturb_continuous(
    fit = fit,
    method = method,
    targets = targets,
    dose = dose,
    remaining_strength = remaining_strength,
    edges = edges,
    combination_size = combination_size,
    steps = steps,
    pulse_values = pulse_values,
    spillover_nodes = spillover_nodes,
    threshold = threshold,
    seed = seed,
    config = config, modules = modules, anchors = anchors,
    symptom_weights = symptom_weights, costs = costs,
    bootstrap_indices = bootstrap_indices,
    supplied = list(dose = !missing(dose), remaining_strength = !missing(remaining_strength),
                    steps = !missing(steps), threshold = !missing(threshold), seed = !missing(seed))
  )
}

#' @export
print.quicknet_perturbation <- function(x, ...) {
  cat("<quicknet_perturbation>\n")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Method: ", x$method, "\n", sep = "")
  if (!is.null(x$report)) {
    cat(x$report, "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.quicknet_perturbation <- function(object, ...) {
  object$metrics
}

#' Plot virtual perturbation results
#'
#' @param perturbation A \code{quicknet_perturbation} object.
#' @param type Plot type. \code{"auto"} selects a sensible default from the
#' perturbation method. Supported values are \code{"rank"},
#' \code{"dose_response"}, \code{"node_change"}, \code{"edge_block"}, and
#' \code{"sequence"}.
#' @param top_n Maximum number of rows shown for ranking-style plots.
#' @param target Optional target label used to select one perturbation for
#' node-level plots. When the target has several conditions, the first is shown;
#' use \code{perturbation_id} to select a particular dose or condition.
#' @param perturbation_id Optional numeric perturbation id used to select one
#' perturbation for node-level plots.
#'
#' @return A \code{ggplot} object. Plots summarize model-implied in silico
#' perturbation results and should not be interpreted as causal intervention
#' effects.
#' @export
get_perturbation_plot <- function(perturbation,
                                  type = c("auto", "rank", "dose_response", "node_change", "edge_block", "sequence"),
                                  top_n = 20,
                                  target = NULL,
                                  perturbation_id = NULL) {
  if (!inherits(perturbation, "quicknet_perturbation")) {
    stop("perturbation must be a quicknet_perturbation object.", call. = FALSE)
  }
  type <- match.arg(type)
  if (!quicknet_is_positive_integer(top_n)) {
    stop("top_n must be a positive integer.", call. = FALSE)
  }
  if (type == "auto") {
    type <- quicknet_perturb_plot_auto_type(perturbation)
  }

  switch(
    type,
    rank = quicknet_perturb_plot_rank(perturbation, top_n = top_n),
    dose_response = quicknet_perturb_plot_dose_response(perturbation),
    node_change = quicknet_perturb_plot_node_change(
      perturbation,
      top_n = top_n,
      target = target,
      perturbation_id = perturbation_id
    ),
    edge_block = quicknet_perturb_plot_edge_block(perturbation, top_n = top_n),
    sequence = quicknet_perturb_plot_sequence(perturbation)
  )
}

#' @export
plot.quicknet_perturbation <- function(x,
                                       type = c("auto", "rank", "dose_response", "node_change", "edge_block", "sequence"),
                                       top_n = 20,
                                       target = NULL,
                                       perturbation_id = NULL,
                                       ...) {
  get_perturbation_plot(
    perturbation = x,
    type = type,
    top_n = top_n,
    target = target,
    perturbation_id = perturbation_id
  )
}

quicknet_perturb_ising <- function(fit,
                                   targets,
                                   threshold_shift,
                                   n_samples,
                                   burnin,
                                   thinning,
                                   seed) {
  if (!quicknet_is_ising_model(fit$model)) {
    stop("method = 'ising_threshold' requires an Ising fit.", call. = FALSE)
  }
  if (!is.numeric(threshold_shift) || length(threshold_shift) != 1 || !is.finite(threshold_shift)) {
    stop("threshold_shift must be a single finite number.", call. = FALSE)
  }
  weight_matrix <- as.matrix(fit$graph)
  node_names <- colnames(weight_matrix)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(ncol(weight_matrix)))
  colnames(weight_matrix) <- rownames(weight_matrix) <- node_names

  thresholds <- fit$nodes$threshold
  names(thresholds) <- fit$nodes$node
  thresholds <- thresholds[node_names]
  if (any(!is.finite(thresholds))) {
    stop("Ising threshold perturbation requires finite node thresholds.", call. = FALSE)
  }

  target_sets <- quicknet_perturb_target_sets(targets, node_names)
  baseline_samples <- quicknet_perturb_ising_gibbs(
    weight_matrix = weight_matrix,
    thresholds = thresholds,
    n_samples = n_samples,
    burnin = burnin,
    thinning = thinning,
    seed = seed
  )
  baseline_activity <- colMeans(baseline_samples)
  baseline_total <- sum(baseline_activity)

  rows <- list()
  perturbation_rows <- list()
  for (target_set in target_sets) {
    perturbed_thresholds <- thresholds
    perturbed_thresholds[target_set] <- perturbed_thresholds[target_set] + threshold_shift
    perturbed_samples <- quicknet_perturb_ising_gibbs(
      weight_matrix = weight_matrix,
      thresholds = perturbed_thresholds,
      n_samples = n_samples,
      burnin = burnin,
      thinning = thinning,
      seed = if (is.null(seed)) NULL else seed + length(rows) + 1
    )
    perturbed_activity <- colMeans(perturbed_samples)
    non_targets <- setdiff(node_names, target_set)
    rows[[length(rows) + 1]] <- data.frame(
      perturbation_type = "ising_threshold",
      target = paste(target_set, collapse = "+"),
      threshold_shift = threshold_shift,
      baseline_activity = baseline_total,
      perturbed_activity = sum(perturbed_activity),
      activity_reduction = baseline_total - sum(perturbed_activity),
      target_activity_reduction = mean(baseline_activity[target_set] - perturbed_activity[target_set]),
      spillover_activity_reduction = ifelse(length(non_targets) > 0, mean(baseline_activity[non_targets] - perturbed_activity[non_targets]), NA_real_),
      adverse_increase_count = sum(perturbed_activity[non_targets] - baseline_activity[non_targets] > 0.05),
      changed_node_count = sum(abs(perturbed_activity - baseline_activity) > 0.05),
      stringsAsFactors = FALSE
    )
    perturbation_rows[[length(perturbation_rows) + 1]] <- data.frame(
      target = paste(target_set, collapse = "+"),
      node = node_names,
      baseline_activity = as.numeric(baseline_activity[node_names]),
      perturbed_activity = as.numeric(perturbed_activity[node_names]),
      activity_change = as.numeric(perturbed_activity[node_names] - baseline_activity[node_names]),
      stringsAsFactors = FALSE
    )
  }

  metrics <- do.call(rbind, rows)
  perturbations <- do.call(rbind, perturbation_rows)
  rankings <- metrics[order(-metrics$activity_reduction), , drop = FALSE]
  rownames(rankings) <- NULL

  quicknet_perturbation_object(
    method = "ising_threshold",
    model = fit$model,
    settings = list(
      threshold_shift = threshold_shift,
      n_samples = n_samples,
      burnin = burnin,
      thinning = thinning,
      seed = seed,
      interpretation = "model-implied NIRA-style threshold perturbation; not a causal intervention effect"
    ),
    baseline = list(activity = baseline_activity, total_activity = baseline_total),
    perturbations = perturbations,
    metrics = metrics,
    rankings = rankings
  )
}

quicknet_perturb_ising_gibbs <- function(weight_matrix,
                                         thresholds,
                                         n_samples,
                                         burnin,
                                         thinning,
                                         seed) {
  if (!quicknet_is_positive_integer(n_samples) || !quicknet_is_positive_integer(thinning)) {
    stop("n_samples and thinning must be positive integers.", call. = FALSE)
  }
  if (!is.numeric(burnin) || length(burnin) != 1 || !is.finite(burnin) ||
      burnin < 0 || burnin != floor(burnin)) {
    stop("burnin must be a non-negative integer.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  weight_matrix <- as.matrix(weight_matrix)
  p <- length(thresholds)
  if (nrow(weight_matrix) != p || ncol(weight_matrix) != p) {
    stop("weight_matrix dimensions must match thresholds length.", call. = FALSE)
  }
  if (any(!is.finite(weight_matrix)) || any(!is.finite(thresholds))) {
    stop("Ising weights and thresholds must be finite.", call. = FALSE)
  }
  state <- stats::rbinom(p, size = 1, prob = 0.50)
  samples <- matrix(NA_integer_, nrow = n_samples, ncol = p)
  colnames(samples) <- names(thresholds)
  total_sweeps <- burnin + n_samples * thinning
  sample_index <- 0L
  for (sweep in seq_len(total_sweeps)) {
    for (node_index in sample.int(p)) {
      linear_predictor <- thresholds[node_index] + sum(weight_matrix[node_index, ] * state)
      state[node_index] <- stats::rbinom(1, size = 1, prob = stats::plogis(linear_predictor))
    }
    if (sweep > burnin && ((sweep - burnin) %% thinning == 0)) {
      sample_index <- sample_index + 1L
      samples[sample_index, ] <- state
    }
  }
  samples
}

quicknet_perturb_target_sets <- function(targets, node_names) {
  if (is.null(targets)) {
    return(lapply(node_names, function(node) node))
  }
  if (is.list(targets)) {
    target_sets <- lapply(targets, as.character)
  } else {
    target_sets <- lapply(as.character(targets), function(node) node)
  }
  if (length(target_sets) == 0) {
    stop("targets must contain at least one node.", call. = FALSE)
  }
  invisible(lapply(target_sets, quicknet_perturb_validate_nodes, node_names = node_names))
  target_sets
}

quicknet_perturb_validate_nodes <- function(nodes, node_names) {
  if (length(nodes) == 0 || anyNA(nodes) || anyDuplicated(nodes)) {
    stop("Each target set must contain one or more unique, non-missing nodes.", call. = FALSE)
  }
  missing_nodes <- setdiff(nodes, node_names)
  if (length(missing_nodes) > 0) {
    stop("Unknown node(s): ", paste(missing_nodes, collapse = ", "), call. = FALSE)
  }
  nodes
}

quicknet_perturb_rank <- function(metrics) {
  preferred <- c("vpps", "incremental_pair_value", "objective", "communication_block", "system_benefit")
  selected <- preferred[preferred %in% names(metrics)]
  if (length(selected)) {
    out <- metrics[order(-metrics[[selected[1]]]), , drop = FALSE]
  } else if ("burden_reduction" %in% names(metrics)) {
    out <- metrics[order(-metrics$burden_reduction), , drop = FALSE]
  } else if ("activity_reduction" %in% names(metrics)) {
    out <- metrics[order(-metrics$activity_reduction), , drop = FALSE]
  } else if ("spillover_blocked" %in% names(metrics)) {
    out <- metrics[order(-metrics$spillover_blocked), , drop = FALSE]
  } else {
    out <- metrics
  }
  rownames(out) <- NULL
  out
}

quicknet_perturb_plot_auto_type <- function(perturbation) {
  if (perturbation$method == "dosage") {
    return("dose_response")
  }
  if (perturbation$method == "edge_block") {
    return("edge_block")
  }
  if (perturbation$method == "sequence") {
    return("sequence")
  }
  "rank"
}

quicknet_perturb_plot_rank <- function(perturbation, top_n) {
  metrics <- perturbation$rankings
  value_column <- quicknet_perturb_plot_metric_column(
    metrics,
    c("vpps", "incremental_pair_value", "objective", "communication_block", "system_benefit", "activity_reduction", "burden_reduction")
  )
  label_column <- quicknet_perturb_plot_label_column(
    metrics,
    c("target", "sequence", "blocked_edge", "chosen_node", "cumulative_targets", "perturbation_type")
  )
  df <- metrics[seq_len(min(nrow(metrics), top_n)), , drop = FALSE]
  df$plot_label <- as.character(df[[label_column]])
  # Each condition needs its own bar; repeated target labels otherwise stack.
  if (anyDuplicated(df$plot_label)) {
    if ("dose" %in% names(df)) {
      df$plot_label <- paste0(df$plot_label, " (dose ", df$dose, ")")
    }
    if (anyDuplicated(df$plot_label) && "perturbation_type" %in% names(df)) {
      df$plot_label <- paste(df$plot_label, df$perturbation_type, sep = " / ")
    }
    df$plot_label <- make.unique(df$plot_label)
  }
  df$plot_value <- as.numeric(df[[value_column]])
  df <- df[order(df$plot_value), , drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(plot_label, plot_value), y = plot_value)) +
    ggplot2::geom_col(fill = "#3b6f8f", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Perturbation ranking",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = NULL,
      y = quicknet_perturb_plot_axis_label(value_column),
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_dose_response <- function(perturbation) {
  if (!perturbation$method %in% c("dosage", "symperturb")) {
    stop("type = 'dose_response' requires a dosage perturbation or symperturb analysis.", call. = FALSE)
  }
  metrics <- perturbation$metrics
  if (perturbation$method == "symperturb") {
    metrics <- perturbation$dose_response
    metrics$dose <- metrics$alpha
  }
  if (!all(c("target", "dose", "system_benefit") %in% names(metrics))) {
    stop("Dose-response plotting requires target, dose, and system_benefit fields.", call. = FALSE)
  }
  df <- metrics
  df$dose_value <- quicknet_perturb_plot_numeric_dose(df$dose)

  ggplot2::ggplot(df, ggplot2::aes(x = dose_value, y = system_benefit, group = target, color = target)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(
      title = "Dose-response simulation",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = "Dose",
      y = "Mean downstream improvement (baseline SD)",
      color = "Target",
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_node_change <- function(perturbation, top_n, target, perturbation_id) {
  df <- perturbation$perturbations
  if (!nrow(df)) stop("No node-level state results are available for this method.", call. = FALSE)
  value_column <- quicknet_perturb_plot_metric_column(df, c("state_change", "activity_change"))
  if (!"node" %in% names(df)) {
    stop("Node-change plotting requires a node-level perturbation table.", call. = FALSE)
  }

  if (!is.null(perturbation_id)) {
    if (!"perturbation_id" %in% names(df)) {
      stop("This perturbation object does not contain perturbation_id values.", call. = FALSE)
    }
    df <- df[df$perturbation_id == perturbation_id, , drop = FALSE]
  } else if (!is.null(target)) {
    if (!"target" %in% names(df)) {
      stop("This perturbation object does not contain target labels.", call. = FALSE)
    }
    df <- df[df$target == target, , drop = FALSE]
  } else if ("perturbation_id" %in% names(df)) {
    df <- df[df$perturbation_id == df$perturbation_id[[1]], , drop = FALSE]
  } else if ("target" %in% names(df)) {
    df <- df[df$target == df$target[[1]], , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No node-level perturbation rows match the requested selection.", call. = FALSE)
  }
  if ("perturbation_id" %in% names(df)) {
    df <- df[df$perturbation_id == df$perturbation_id[[1]], , drop = FALSE]
  }
  df$plot_value <- as.numeric(df[[value_column]])
  df <- df[order(-abs(df$plot_value)), , drop = FALSE]
  df <- df[seq_len(min(nrow(df), top_n)), , drop = FALSE]
  df$direction <- ifelse(df$plot_value >= 0, "Increase", "Decrease")

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(node, plot_value), y = plot_value, fill = direction)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(Increase = "#b85c38", Decrease = "#3f7f5f")) +
    ggplot2::labs(
      title = "Node-level perturbation change",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = NULL,
      y = quicknet_perturb_plot_axis_label(value_column),
      fill = NULL,
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_edge_block <- function(perturbation, top_n) {
  if (perturbation$method != "edge_block") {
    stop("type = 'edge_block' requires an edge_block perturbation.", call. = FALSE)
  }
  metrics <- perturbation$metrics
  if (!all(c("blocked_edge", "communication_block") %in% names(metrics))) {
    stop("Edge-block plotting requires blocked_edge and communication_block fields.", call. = FALSE)
  }
  df <- metrics[seq_len(min(nrow(metrics), top_n)), , drop = FALSE]
  df$plot_value <- as.numeric(df$communication_block)
  df <- df[order(df$plot_value), , drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(blocked_edge, plot_value), y = plot_value)) +
    ggplot2::geom_col(fill = "#79553d", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Edge communication blocking",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = "Blocked edge",
      y = "Relative propagation loss",
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_sequence <- function(perturbation) {
  paths <- perturbation$sequence_paths
  if (!length(paths)) stop("Sequence plotting requires computed sequence paths.", call. = FALSE)
  df <- paths[[1]]
  ggplot2::ggplot(df, ggplot2::aes(x = step, y = system_benefit)) +
    ggplot2::geom_col(ggplot2::aes(y = marginal_benefit), fill = "#9a7b4f", alpha = 0.55) +
    ggplot2::geom_line(color = "#2f5d7c", linewidth = 0.8) +
    ggplot2::geom_point(color = "#2f5d7c", size = 2.4) +
    ggplot2::scale_x_continuous(breaks = df$step, labels = paste(df$step, df$chosen_node)) +
    ggplot2::labs(title = "Highest-objective beam-search sequence",
                  subtitle = quicknet_perturb_plot_subtitle(perturbation),
                  x = "Step and selected node", y = "Mean downstream improvement (baseline SD)",
                  caption = paste("Line: cumulative set benefit; bars: marginal benefit.",
                                  quicknet_perturb_plot_caption())) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_metric_column <- function(df, candidates) {
  matched <- candidates[candidates %in% names(df)]
  if (length(matched) == 0) {
    stop("No supported perturbation metric column is available for this plot.", call. = FALSE)
  }
  matched[[1]]
}

quicknet_perturb_plot_label_column <- function(df, candidates) {
  matched <- candidates[candidates %in% names(df)]
  if (length(matched) == 0) {
    stop("No supported perturbation label column is available for this plot.", call. = FALSE)
  }
  matched[[1]]
}

quicknet_perturb_plot_numeric_dose <- function(dose) {
  vapply(strsplit(as.character(dose), "\\+"), function(parts) {
    sum(as.numeric(parts))
  }, numeric(1))
}

quicknet_perturb_plot_axis_label <- function(column) {
  labels <- c(
    burden_reduction = "Burden reduction",
    activity_reduction = "Activity reduction",
    spillover_blocked = "Spillover blocked",
    vpps = "VPPS (within candidate set)",
    incremental_pair_value = "Increment beyond the better single target (SD)",
    objective = "Discounted benefit minus cost",
    communication_block = "Relative propagation loss",
    system_benefit = "Mean downstream improvement (baseline SD)",
    state_change = "State change",
    activity_change = "Activity change"
  )
  if (column %in% names(labels)) labels[[column]] else column
}

quicknet_perturb_plot_subtitle <- function(perturbation) {
  if (!is.null(perturbation$metadata$reference_version)) {
    return(paste0("SymPerturb Gaussian model (ridge = ", perturbation$settings$ridge,
                  "); ", perturbation$method))
  }
  paste0(perturbation$model, " model, ", perturbation$method, " perturbation")
}

quicknet_perturb_plot_caption <- function() {
  "Model-implied in silico simulation; not a causal intervention effect."
}

quicknet_perturbation_object <- function(method,
                                         model,
                                         settings,
                                         baseline,
                                         perturbations,
                                         metrics,
                                         rankings) {
  report <- paste0(
    "Computed ", method, " perturbation for a ", model,
    " model. Results are model-implied in silico simulations and should not be interpreted as causal intervention effects."
  )
  structure(
    list(
      method = method,
      model = model,
      settings = settings,
      baseline = baseline,
      perturbations = perturbations,
      metrics = metrics,
      rankings = rankings,
      report = report
    ),
    class = "quicknet_perturbation"
  )
}
