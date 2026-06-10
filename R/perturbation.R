#' Virtual perturbation and intervention simulation
#'
#' @param fit A \code{quicknet_fit} object.
#' @param method Perturbation method. Continuous networks support
#' \code{"dosage"}, \code{"knockout"}, \code{"knockdown"},
#' \code{"edge_block"}, \code{"combination"}, and \code{"sequence"}.
#' Ising networks support \code{"ising_threshold"}.
#' @param targets Target node names. If \code{NULL}, all nodes are considered.
#' @param dose Numeric perturbation dose for Gaussian conditioning. Positive
#' values are interpreted as reductions and internally applied as negative
#' target states.
#' @param remaining_strength Fraction of incident precision entries retained
#' for knockdown. Use \code{0} for structural knockout.
#' @param edges Optional edge table for \code{method = "edge_block"}. It can
#' contain \code{from/to} or \code{node_i/node_j} columns. If \code{NULL}, all
#' nonzero edges are considered.
#' @param combination_size Number of targets in each combination.
#' @param steps Number of greedy sequence steps.
#' @param threshold_shift Threshold shift for Ising perturbation. Negative
#' values lower target activation tendency.
#' @param n_samples Number of Gibbs samples for Ising perturbation.
#' @param burnin Number of burn-in sweeps for Ising perturbation.
#' @param thinning Thinning interval for Ising perturbation.
#' @param seed Random seed.
#' @param pulse_values Named vector used by \code{method = "edge_block"} to
#' define the source pulse.
#' @param spillover_nodes Optional nodes used to summarize spillover in
#' \code{method = "edge_block"}.
#' @param threshold Absolute threshold used to define nonzero edges.
#'
#' @return A \code{quicknet_perturbation} object. Results are model-implied
#' in silico simulations and should not be interpreted as causal intervention
#' effects.
#' @export
Perturbation <- function(fit,
                         method = c("dosage", "knockout", "knockdown", "edge_block", "combination", "sequence", "ising_threshold"),
                         targets = NULL,
                         dose = c(0.25, 0.50, 0.75, 1.00),
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
                         threshold = 1e-10) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  method <- if (missing(method)) {
    if (fit$model == "ising") "ising_threshold" else "dosage"
  } else {
    match.arg(method)
  }

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
    seed = seed
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
#' node-level plots.
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

quicknet_perturb_continuous <- function(fit,
                                        method,
                                        targets,
                                        dose,
                                        remaining_strength,
                                        edges,
                                        combination_size,
                                        steps,
                                        pulse_values,
                                        spillover_nodes,
                                        threshold,
                                        seed) {
  if (!fit$model %in% c("EBICglasso", "correlation", "partial", "ordinal")) {
    stop(
      "Continuous perturbation currently supports EBICglasso, correlation, partial, and ordinal fits. ",
      "Use method = 'ising_threshold' for Ising fits.",
      call. = FALSE
    )
  }

  precision <- quicknet_perturb_precision(fit)
  node_names <- colnames(precision)
  target_sets <- quicknet_perturb_target_sets(targets, node_names)
  baseline_state <- stats::setNames(rep(0, length(node_names)), node_names)

  if (method == "dosage") {
    evaluations <- unlist(lapply(target_sets, function(target_set) {
      lapply(dose, function(one_dose) {
        quicknet_perturb_evaluate_gaussian(
          precision_matrix = precision,
          target_nodes = target_set,
          dose = one_dose,
          perturbation_type = "gaussian_conditioning_dosage",
          baseline_state = baseline_state
        )
      })
    }), recursive = FALSE)
    bound <- quicknet_perturb_bind_evaluations(evaluations)
    metrics <- bound$metrics
    perturbations <- bound$perturbations
  } else if (method == "knockout") {
    evaluations <- list()
    for (target_set in target_sets) {
      evaluations[[length(evaluations) + 1]] <- quicknet_perturb_evaluate_gaussian(
        precision_matrix = precision,
        target_nodes = target_set,
        dose = 1,
        perturbation_type = "conditional_state_vKO",
        baseline_state = baseline_state
      )
      if (length(target_set) == 1) {
        structural_precision <- quicknet_perturb_attenuate_precision(precision, target_set, remaining_strength = 0)
        evaluations[[length(evaluations) + 1]] <- quicknet_perturb_evaluate_gaussian(
          precision_matrix = structural_precision,
          target_nodes = target_set,
          dose = 1,
          perturbation_type = "precision_edge_vKO",
          baseline_state = baseline_state
        )
      }
    }
    bound <- quicknet_perturb_bind_evaluations(evaluations)
    metrics <- bound$metrics
    perturbations <- bound$perturbations
  } else if (method == "knockdown") {
    evaluations <- lapply(target_sets, function(target_set) {
      if (length(target_set) != 1) {
        stop("knockdown currently expects single-node target sets.", call. = FALSE)
      }
      modified_precision <- quicknet_perturb_attenuate_precision(precision, target_set, remaining_strength = remaining_strength)
      quicknet_perturb_evaluate_gaussian(
        precision_matrix = modified_precision,
        target_nodes = target_set,
        dose = 1 - remaining_strength,
        perturbation_type = paste0("precision_edge_vKD_", round(remaining_strength * 100), "_percent_remaining"),
        baseline_state = baseline_state
      )
    })
    bound <- quicknet_perturb_bind_evaluations(evaluations)
    metrics <- bound$metrics
    perturbations <- bound$perturbations
  } else if (method == "edge_block") {
    edge_result <- quicknet_perturb_edge_block(
      precision_matrix = precision,
      fit = fit,
      targets = targets,
      edges = edges,
      pulse_values = pulse_values,
      spillover_nodes = spillover_nodes,
      threshold = threshold
    )
    metrics <- edge_result$metrics
    perturbations <- edge_result$perturbations
  } else if (method == "combination") {
    combo_result <- quicknet_perturb_combination(
      precision_matrix = precision,
      targets = targets,
      dose = dose[[1]],
      combination_size = combination_size
    )
    metrics <- combo_result$metrics
    perturbations <- combo_result$perturbations
  } else if (method == "sequence") {
    sequence_result <- quicknet_perturb_sequence(
      precision_matrix = precision,
      targets = targets,
      dose = dose[[1]],
      steps = steps
    )
    metrics <- sequence_result$metrics
    perturbations <- sequence_result$perturbations
  }

  rankings <- quicknet_perturb_rank(metrics)
  quicknet_perturbation_object(
    method = method,
    model = fit$model,
    settings = list(
      dose = dose,
      remaining_strength = remaining_strength,
      combination_size = combination_size,
      steps = steps,
      threshold = threshold,
      seed = seed,
      interpretation = "model-implied in silico simulation; not a causal intervention effect"
    ),
    baseline = baseline_state,
    perturbations = perturbations,
    metrics = metrics,
    rankings = rankings
  )
}

quicknet_perturb_ising <- function(fit,
                                   targets,
                                   threshold_shift,
                                   n_samples,
                                   burnin,
                                   thinning,
                                   seed) {
  if (fit$model != "ising") {
    stop("method = 'ising_threshold' requires an Ising fit.", call. = FALSE)
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
      seed = seed + length(rows) + 1
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

quicknet_perturb_precision <- function(fit) {
  mat <- as.matrix(fit$graph)
  node_names <- colnames(mat)
  if (is.null(node_names)) node_names <- rownames(mat)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(ncol(mat)))
  colnames(mat) <- rownames(mat) <- node_names

  if (fit$model == "correlation") {
    precision <- solve(quicknet_make_positive_definite(mat))
  } else {
    precision <- -mat
    diag(precision) <- 1
  }
  precision <- quicknet_perturb_make_positive_definite(precision)
  colnames(precision) <- rownames(precision) <- node_names
  precision
}

quicknet_perturb_conditioned_state <- function(precision_matrix, target_values) {
  precision_matrix <- quicknet_perturb_make_positive_definite(precision_matrix)
  node_names <- colnames(precision_matrix)
  target_nodes <- names(target_values)
  if (is.null(target_nodes) || any(!target_nodes %in% node_names)) {
    stop("target_values must be a named numeric vector with valid node names.", call. = FALSE)
  }
  non_targets <- setdiff(node_names, target_nodes)
  final_state <- stats::setNames(rep(0, length(node_names)), node_names)
  final_state[target_nodes] <- target_values
  if (length(non_targets) == 0) {
    return(final_state)
  }

  covariance_matrix <- solve(precision_matrix)
  covariance_matrix <- (covariance_matrix + t(covariance_matrix)) / 2
  sigma_nt_t <- covariance_matrix[non_targets, target_nodes, drop = FALSE]
  sigma_t_t <- covariance_matrix[target_nodes, target_nodes, drop = FALSE]
  conditional_mean <- sigma_nt_t %*% solve(sigma_t_t, matrix(target_values, ncol = 1))
  final_state[non_targets] <- as.numeric(conditional_mean)
  final_state
}

quicknet_perturb_evaluate_gaussian <- function(precision_matrix,
                                               target_nodes,
                                               dose,
                                               perturbation_type,
                                               baseline_state) {
  node_names <- colnames(precision_matrix)
  target_values <- stats::setNames(-abs(rep(dose, length.out = length(target_nodes))), target_nodes)
  final_state <- quicknet_perturb_conditioned_state(precision_matrix, target_values)
  non_targets <- setdiff(node_names, target_nodes)
  metrics <- data.frame(
    perturbation_type = perturbation_type,
    target = paste(target_nodes, collapse = "+"),
    dose = paste(abs(rep(dose, length.out = length(target_nodes))), collapse = "+"),
    final_burden = sum(final_state),
    burden_reduction = sum(baseline_state) - sum(final_state),
    target_reduction = mean(baseline_state[target_nodes] - final_state[target_nodes]),
    spillover_reduction = ifelse(length(non_targets) > 0, mean(baseline_state[non_targets] - final_state[non_targets]), NA_real_),
    adverse_increase_count = sum(final_state[non_targets] - baseline_state[non_targets] > 0.05),
    changed_node_count = sum(abs(final_state - baseline_state) > 0.05),
    stringsAsFactors = FALSE
  )
  list(metrics = metrics, final_state = final_state)
}

quicknet_perturb_bind_evaluations <- function(evaluations) {
  metrics <- do.call(rbind, lapply(evaluations, `[[`, "metrics"))
  rownames(metrics) <- NULL
  perturbations <- do.call(rbind, lapply(seq_along(evaluations), function(eval_index) {
    final_state <- evaluations[[eval_index]]$final_state
    metric <- evaluations[[eval_index]]$metrics
    data.frame(
      perturbation_id = eval_index,
      perturbation_type = metric$perturbation_type[[1]],
      target = metric$target[[1]],
      node = names(final_state),
      baseline_state = 0,
      final_state = as.numeric(final_state),
      state_change = as.numeric(final_state),
      stringsAsFactors = FALSE
    )
  }))
  rownames(perturbations) <- NULL
  list(metrics = metrics, perturbations = perturbations)
}

quicknet_perturb_attenuate_precision <- function(precision_matrix, target_node, remaining_strength) {
  modified <- as.matrix(precision_matrix)
  if (!target_node %in% colnames(modified)) {
    stop("target_node not found in precision_matrix: ", target_node, call. = FALSE)
  }
  modified[target_node, ] <- modified[target_node, ] * remaining_strength
  modified[, target_node] <- modified[, target_node] * remaining_strength
  modified[target_node, target_node] <- precision_matrix[target_node, target_node]
  quicknet_perturb_make_positive_definite(modified)
}

quicknet_perturb_block_precision_edge <- function(precision_matrix, node_i, node_j) {
  modified <- as.matrix(precision_matrix)
  if (!all(c(node_i, node_j) %in% colnames(modified))) {
    stop("node_i or node_j is not present in precision_matrix.", call. = FALSE)
  }
  modified[node_i, node_j] <- 0
  modified[node_j, node_i] <- 0
  quicknet_perturb_make_positive_definite(modified)
}

quicknet_perturb_make_positive_definite <- function(mat) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  mat <- (mat + t(mat)) / 2
  eigen_values <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigen_values) > 1e-8) {
    return(mat)
  }
  adjusted <- as.matrix(Matrix::nearPD(mat, corr = FALSE)$mat)
  (adjusted + t(adjusted)) / 2
}

quicknet_perturb_spillover_from_pulse <- function(precision_matrix, pulse_values, spillover_nodes = NULL) {
  final_state <- quicknet_perturb_conditioned_state(precision_matrix, pulse_values)
  if (is.null(spillover_nodes)) {
    spillover_nodes <- setdiff(names(final_state), names(pulse_values))
  }
  list(
    final_state = final_state,
    spillover_sum = sum(final_state[spillover_nodes]),
    spillover_abs_sum = sum(abs(final_state[spillover_nodes]))
  )
}

quicknet_perturb_edge_block <- function(precision_matrix,
                                        fit,
                                        targets,
                                        edges,
                                        pulse_values,
                                        spillover_nodes,
                                        threshold) {
  node_names <- colnames(precision_matrix)
  if (is.null(pulse_values)) {
    pulse_nodes <- if (is.null(targets)) {
      strongest <- fit$nodes$node[which.max(fit$nodes$strength)]
      strongest
    } else {
      unlist(quicknet_perturb_target_sets(targets, node_names)[[1]])
    }
    pulse_values <- stats::setNames(rep(0.60, length(pulse_nodes)), pulse_nodes)
  }
  if (is.null(spillover_nodes)) {
    spillover_nodes <- setdiff(node_names, names(pulse_values))
  }
  edge_table <- quicknet_perturb_edge_candidates(fit, edges = edges, threshold = threshold)
  unblocked <- quicknet_perturb_spillover_from_pulse(precision_matrix, pulse_values, spillover_nodes)

  rows <- list()
  for (edge_index in seq_len(nrow(edge_table))) {
    node_i <- edge_table$node_i[edge_index]
    node_j <- edge_table$node_j[edge_index]
    blocked_precision <- quicknet_perturb_block_precision_edge(precision_matrix, node_i, node_j)
    blocked <- quicknet_perturb_spillover_from_pulse(blocked_precision, pulse_values, spillover_nodes)
    rows[[length(rows) + 1]] <- data.frame(
      perturbation_type = "precision_edge_block",
      blocked_edge = paste(node_i, node_j, sep = "--"),
      node_i = node_i,
      node_j = node_j,
      weight = edge_table$weight[edge_index],
      abs_weight = abs(edge_table$weight[edge_index]),
      unblocked_spillover_sum = unblocked$spillover_sum,
      blocked_spillover_sum = blocked$spillover_sum,
      spillover_blocked = unblocked$spillover_sum - blocked$spillover_sum,
      unblocked_abs_spillover = unblocked$spillover_abs_sum,
      blocked_abs_spillover = blocked$spillover_abs_sum,
      changed_node_count = sum(abs(blocked$final_state - unblocked$final_state) > 0.05),
      stringsAsFactors = FALSE
    )
  }
  metrics <- do.call(rbind, rows)
  metrics <- metrics[order(-metrics$spillover_blocked, -metrics$abs_weight), , drop = FALSE]
  rownames(metrics) <- NULL
  list(metrics = metrics, perturbations = edge_table)
}

quicknet_perturb_combination <- function(precision_matrix, targets, dose, combination_size) {
  node_names <- colnames(precision_matrix)
  target_nodes <- if (is.null(targets)) node_names else unlist(targets)
  target_nodes <- unique(target_nodes)
  quicknet_perturb_validate_nodes(target_nodes, node_names)
  if (length(target_nodes) < combination_size) {
    stop("Not enough target nodes for the requested combination_size.", call. = FALSE)
  }
  baseline <- stats::setNames(rep(0, length(node_names)), node_names)
  single <- lapply(target_nodes, function(node) {
    quicknet_perturb_evaluate_gaussian(precision_matrix, node, dose, "single_dosage_reference", baseline)$metrics
  })
  single_table <- do.call(rbind, single)
  single_lookup <- stats::setNames(single_table$burden_reduction, single_table$target)

  combinations <- utils::combn(target_nodes, combination_size, simplify = FALSE)
  rows <- lapply(combinations, function(target_set) {
    metrics <- quicknet_perturb_evaluate_gaussian(precision_matrix, target_set, dose, "gaussian_combination_dosage", baseline)$metrics
    expected_additive <- sum(single_lookup[target_set])
    metrics$expected_additive_reduction <- expected_additive
    metrics$synergy <- metrics$burden_reduction - expected_additive
    metrics
  })
  metrics <- do.call(rbind, rows)
  metrics <- metrics[order(-metrics$burden_reduction, -metrics$synergy), , drop = FALSE]
  rownames(metrics) <- NULL
  list(metrics = metrics, perturbations = single_table)
}

quicknet_perturb_sequence <- function(precision_matrix, targets, dose, steps) {
  node_names <- colnames(precision_matrix)
  remaining_nodes <- if (is.null(targets)) node_names else unique(unlist(targets))
  quicknet_perturb_validate_nodes(remaining_nodes, node_names)
  baseline <- stats::setNames(rep(0, length(node_names)), node_names)
  selected_nodes <- character()
  previous_reduction <- 0
  rows <- list()

  for (step in seq_len(min(steps, length(remaining_nodes)))) {
    candidate_rows <- lapply(remaining_nodes, function(node) {
      candidate_targets <- c(selected_nodes, node)
      metrics <- quicknet_perturb_evaluate_gaussian(
        precision_matrix,
        candidate_targets,
        dose,
        paste0("greedy_gaussian_sequence_step_", step),
        baseline
      )$metrics
      metrics$next_node <- node
      metrics$incremental_burden_reduction <- metrics$burden_reduction - previous_reduction
      metrics
    })
    candidates <- do.call(rbind, candidate_rows)
    best <- candidates[order(-candidates$incremental_burden_reduction, -candidates$burden_reduction), , drop = FALSE][1, ]
    chosen_node <- best$next_node[[1]]
    selected_nodes <- c(selected_nodes, chosen_node)
    remaining_nodes <- setdiff(remaining_nodes, chosen_node)
    previous_reduction <- best$burden_reduction[[1]]
    best$step <- step
    best$chosen_node <- chosen_node
    best$cumulative_targets <- paste(selected_nodes, collapse = "+")
    rows[[length(rows) + 1]] <- best
  }

  metrics <- do.call(rbind, rows)
  rownames(metrics) <- NULL
  list(metrics = metrics, perturbations = metrics)
}

quicknet_perturb_ising_gibbs <- function(weight_matrix,
                                         thresholds,
                                         n_samples,
                                         burnin,
                                         thinning,
                                         seed) {
  if (!is.null(seed)) set.seed(seed)
  weight_matrix <- as.matrix(weight_matrix)
  p <- length(thresholds)
  if (nrow(weight_matrix) != p || ncol(weight_matrix) != p) {
    stop("weight_matrix dimensions must match thresholds length.", call. = FALSE)
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
  invisible(lapply(target_sets, quicknet_perturb_validate_nodes, node_names = node_names))
  target_sets
}

quicknet_perturb_validate_nodes <- function(nodes, node_names) {
  missing_nodes <- setdiff(nodes, node_names)
  if (length(missing_nodes) > 0) {
    stop("Unknown node(s): ", paste(missing_nodes, collapse = ", "), call. = FALSE)
  }
  nodes
}

quicknet_perturb_state_table <- function(precision_matrix, metrics) {
  node_names <- colnames(precision_matrix)
  rows <- list()
  for (row_index in seq_len(nrow(metrics))) {
    target_nodes <- unlist(strsplit(metrics$target[row_index], "\\+", fixed = FALSE))
    dose <- as.numeric(unlist(strsplit(as.character(metrics$dose[row_index]), "\\+", fixed = FALSE)))
    target_values <- stats::setNames(-abs(rep(dose, length.out = length(target_nodes))), target_nodes)
    final_state <- quicknet_perturb_conditioned_state(precision_matrix, target_values)
    rows[[length(rows) + 1]] <- data.frame(
      perturbation_id = row_index,
      perturbation_type = metrics$perturbation_type[row_index],
      target = metrics$target[row_index],
      node = node_names,
      baseline_state = 0,
      final_state = as.numeric(final_state[node_names]),
      state_change = as.numeric(final_state[node_names]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

quicknet_perturb_edge_candidates <- function(fit, edges, threshold) {
  if (!is.null(edges)) {
    edge_table <- as.data.frame(edges)
    if (all(c("from", "to") %in% names(edge_table))) {
      edge_table$node_i <- edge_table$from
      edge_table$node_j <- edge_table$to
    }
    if (!all(c("node_i", "node_j") %in% names(edge_table))) {
      stop("edges must contain from/to or node_i/node_j columns.", call. = FALSE)
    }
    if (!"weight" %in% names(edge_table)) {
      edge_table$weight <- fit$graph[cbind(match(edge_table$node_i, rownames(fit$graph)), match(edge_table$node_j, colnames(fit$graph)))]
    }
    return(edge_table[, c("node_i", "node_j", "weight"), drop = FALSE])
  }

  edge_table <- quicknet_edge_table(fit$graph, drop_zero = TRUE, threshold = threshold)
  out <- data.frame(
    node_i = edge_table$from,
    node_j = edge_table$to,
    weight = edge_table$weight,
    stringsAsFactors = FALSE
  )
  out[order(-abs(out$weight)), , drop = FALSE]
}

quicknet_perturb_rank <- function(metrics) {
  if ("burden_reduction" %in% names(metrics)) {
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
    c("burden_reduction", "activity_reduction", "spillover_blocked", "synergy")
  )
  label_column <- quicknet_perturb_plot_label_column(
    metrics,
    c("target", "blocked_edge", "chosen_node", "cumulative_targets", "perturbation_type")
  )
  df <- metrics[seq_len(min(nrow(metrics), top_n)), , drop = FALSE]
  df$plot_label <- as.character(df[[label_column]])
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
  if (perturbation$method != "dosage") {
    stop("type = 'dose_response' requires a dosage perturbation.", call. = FALSE)
  }
  metrics <- perturbation$metrics
  if (!all(c("target", "dose", "burden_reduction") %in% names(metrics))) {
    stop("Dose-response plotting requires target, dose, and burden_reduction fields.", call. = FALSE)
  }
  df <- metrics
  df$dose_value <- quicknet_perturb_plot_numeric_dose(df$dose)

  ggplot2::ggplot(df, ggplot2::aes(x = dose_value, y = burden_reduction, group = target, color = target)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(
      title = "Dose-response simulation",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = "Dose",
      y = "Burden reduction",
      color = "Target",
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_node_change <- function(perturbation, top_n, target, perturbation_id) {
  df <- perturbation$perturbations
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
  if (!all(c("blocked_edge", "spillover_blocked") %in% names(metrics))) {
    stop("Edge-block plotting requires blocked_edge and spillover_blocked fields.", call. = FALSE)
  }
  df <- metrics[seq_len(min(nrow(metrics), top_n)), , drop = FALSE]
  df$plot_value <- as.numeric(df$spillover_blocked)
  df <- df[order(df$plot_value), , drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(blocked_edge, plot_value), y = plot_value)) +
    ggplot2::geom_col(fill = "#79553d", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Edge-block spillover simulation",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = "Blocked edge",
      y = "Spillover blocked",
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

quicknet_perturb_plot_sequence <- function(perturbation) {
  if (perturbation$method != "sequence") {
    stop("type = 'sequence' requires a sequence perturbation.", call. = FALSE)
  }
  metrics <- perturbation$metrics
  if (!all(c("step", "chosen_node", "burden_reduction", "incremental_burden_reduction") %in% names(metrics))) {
    stop("Sequence plotting requires step, chosen_node, burden_reduction, and incremental_burden_reduction fields.", call. = FALSE)
  }
  df <- metrics
  df$step_label <- paste0(df$step, ". ", df$chosen_node)

  ggplot2::ggplot(df, ggplot2::aes(x = step, y = burden_reduction)) +
    ggplot2::geom_col(ggplot2::aes(y = incremental_burden_reduction), fill = "#9a7b4f", alpha = 0.55, width = 0.62) +
    ggplot2::geom_line(color = "#2f5d7c", linewidth = 0.8) +
    ggplot2::geom_point(color = "#2f5d7c", size = 2.4) +
    ggplot2::scale_x_continuous(breaks = df$step, labels = df$step_label) +
    ggplot2::labs(
      title = "Greedy perturbation sequence",
      subtitle = quicknet_perturb_plot_subtitle(perturbation),
      x = "Step and selected node",
      y = "Burden reduction",
      caption = quicknet_perturb_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
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
    synergy = "Synergy",
    state_change = "State change",
    activity_change = "Activity change"
  )
  if (column %in% names(labels)) labels[[column]] else column
}

quicknet_perturb_plot_subtitle <- function(perturbation) {
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
