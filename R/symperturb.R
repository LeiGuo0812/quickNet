quicknet_sym_evaluate <- function(ctx, targets, alpha, type, id) {
  post <- quicknet_sym_post(ctx, targets, alpha)
  observed <- quicknet_sym_observed(post$mean, post$covariance, ctx$config$bounds)
  delta <- (ctx$observed - observed) / ctx$denominator
  other <- setdiff(ctx$nodes, targets)
  label <- paste(targets, collapse = "+")
  metrics <- data.frame(perturbation_id = id, perturbation_type = type, target = label, dose = alpha,
    system_benefit = quicknet_sym_benefit(ctx, delta, other),
    direct_target_benefit = mean(delta[targets]),
    beneficial_spillover_mean = if (length(other)) mean(pmax(delta[other], 0)) else 0,
    adverse_spillover_mean = if (length(other)) mean(pmax(-delta[other], 0)) else 0,
    breadth = if (length(other)) mean(delta[other] >= ctx$config$breadth_threshold) else 0,
    final_burden = sum(observed), burden_reduction = sum(ctx$observed - observed),
    target_reduction = mean(ctx$observed[targets] - observed[targets]),
    spillover_reduction = if (length(other)) mean(ctx$observed[other] - observed[other]) else 0,
    row.names = NULL)
  nodes <- data.frame(perturbation_id = id, perturbation_type = type, target = label,
    node = ctx$nodes, is_target = ctx$nodes %in% targets,
    baseline_state = as.numeric(ctx$observed), final_state = as.numeric(observed),
    state_change = as.numeric(observed - ctx$observed), standardized_improvement = as.numeric(delta),
    latent_baseline_mean = as.numeric(ctx$network$mu), latent_post_mean = as.numeric(post$mean),
    baseline_variance = diag(ctx$network$covariance), post_variance = diag(post$covariance), row.names = NULL)
  list(metrics = metrics, nodes = nodes, moments = post)
}

quicknet_sym_topology <- function(ctx, method, targets, edges) {
  W <- ctx$network$adjacency
  cfg <- ctx$config
  if (method == "node_block") {
    sets <- lapply(targets, function(x) match(x, ctx$nodes))
    labels <- targets
  } else {
    if (is.null(edges)) {
      idx <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
      idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
      idx <- idx[ctx$nodes[idx[, 1]] %in% targets | ctx$nodes[idx[, 2]] %in% targets, , drop = FALSE]
      edges <- data.frame(node_i = ctx$nodes[idx[, 1]], node_j = ctx$nodes[idx[, 2]])
    } else {
      edges <- as.data.frame(edges)
      if (all(c("from", "to") %in% names(edges))) {
        edges$node_i <- edges$from; edges$node_j <- edges$to
      }
      if (!all(c("node_i", "node_j") %in% names(edges))) stop("edges must contain from/to or node_i/node_j columns.", call. = FALSE)
      for (i in seq_len(nrow(edges))) quicknet_perturb_validate_nodes(as.character(unlist(edges[i, c("node_i", "node_j")])), ctx$nodes)
    }
    sets <- lapply(seq_len(nrow(edges)), function(i) match(c(edges$node_i[i], edges$node_j[i]), ctx$nodes))
    labels <- paste(edges$node_i, edges$node_j, sep = " -- ")
  }
  base <- quicknet_sym_potential(W, cfg)
  matrices <- lapply(sets, function(s) quicknet_sym_block(W, s, cfg$block_fraction, method == "edge_block"))
  after <- vapply(matrices, quicknet_sym_potential, numeric(1), config = cfg)
  tab <- data.frame(target = labels, block_fraction = rep(cfg$block_fraction, length(sets)),
                    baseline_propagation = rep(base, length(sets)), blocked_propagation = after,
                    propagation_loss = base - after,
                    communication_block = if (abs(base) < 1e-15) rep(0, length(sets)) else (base - after) / base)
  if (method == "edge_block") {
    tab$blocked_edge <- labels
    tab$node_i <- as.character(edges$node_i); tab$node_j <- as.character(edges$node_j)
  }
  list(metrics = tab, matrices = matrices)
}

quicknet_perturb_continuous <- function(fit, method, targets, dose, remaining_strength,
                                       edges, combination_size, steps, pulse_values,
                                       spillover_nodes, threshold, seed, config,
                                       modules, anchors, symptom_weights, costs,
                                       bootstrap_indices, supplied) {
  if (!is.null(pulse_values) || !is.null(spillover_nodes)) {
    stop("pulse_values and spillover_nodes belong to the retired pulse-conditioning method. SymPerturb edge_block uses adjacency propagation; configure block_fraction and propagation_* in config.", call. = FALSE)
  }
  cfg <- quicknet_sym_config(config)
  if (supplied$threshold) cfg$edge_threshold <- threshold
  if (supplied$seed) cfg$random_seed <- seed
  if (supplied$dose && method %in% c("dosage", "symperturb")) cfg$dose_grid <- dose
  if (method == "sequence") {
    if (supplied$steps || cfg$sequence_length == 0) cfg$sequence_length <- steps
  } else if (method == "symperturb" && supplied$steps) cfg$sequence_length <- steps
  cfg <- quicknet_sym_config(cfg)
  if (method == "sequence" && cfg$sequence_length < 1L) stop("sequence_length must be positive for method = 'sequence'.", call. = FALSE)
  if (method %in% c("combination", "sequence", "knockout") && supplied$dose &&
      (!is.numeric(dose) || length(dose) != 1L || !isTRUE(dose == 1))) stop("This SymPerturb method uses unit dose = 1; use dosage or knockdown for partial doses.", call. = FALSE)
  if (method == "combination" && (!is.numeric(combination_size) || length(combination_size) != 1L || !isTRUE(combination_size == 2))) stop("SymPerturb combination requires combination_size = 2.", call. = FALSE)
  if (method == "knockdown") {
    if (supplied$dose && supplied$remaining_strength) stop("Supply dose or remaining_strength, not both.", call. = FALSE)
    if (!supplied$dose) {
      if (!is.numeric(remaining_strength) || length(remaining_strength) != 1L || !is.finite(remaining_strength) || remaining_strength < 0 || remaining_strength > 1) stop("remaining_strength must be a finite fraction in [0, 1].", call. = FALSE)
      dose <- 1 - remaining_strength
    }
    quicknet_sym_validate_dose(dose)
  }
  nodes <- colnames(fit$graph)
  dat <- fit$data
  if (is.null(dat) || !all(nodes %in% colnames(dat))) stop("SymPerturb requires original numeric participant data in fit$data, aligned to the network nodes.", call. = FALSE)
  dat <- dat[, nodes, drop = FALSE]
  if (method %in% c("symperturb", "sequence") && is.null(modules)) {
    groups <- fit$meta$plot$groups
    if ((is.character(groups) || is.factor(groups)) && length(groups) == length(nodes)) {
      modules <- if (is.null(names(groups))) stats::setNames(as.character(groups), nodes) else groups
    }
    if (is.null(modules)) stop("method = '", method, "' requires a named modules vector with at least two modules.", call. = FALSE)
  }
  ctx <- quicknet_sym_context(dat, cfg, anchors, symptom_weights, modules, costs)
  target_sets <- quicknet_perturb_target_sets(targets, nodes)
  candidates <- unique(unlist(target_sets, use.names = FALSE))
  if (method %in% c("combination", "sequence", "node_block", "edge_block", "symperturb") && is.list(targets)) stop("This method requires a vector of candidate node names, not a list of target sets.", call. = FALSE)
  if (method %in% c("symperturb", "combination", "sequence") && length(candidates) < 2L) stop("At least two candidate targets are required.", call. = FALSE)
  evaluations <- list()
  extra <- list()
  if (method %in% c("dosage", "knockout", "knockdown")) {
    doses <- switch(method, dosage = cfg$dose_grid, knockout = 1, knockdown = dose)
    type <- switch(method, dosage = "state_vDP", knockout = "state_vKO", knockdown = "state_vKD")
    for (s in target_sets) for (a in doses) {
      evaluations[[length(evaluations) + 1L]] <- quicknet_sym_evaluate(ctx, s, a, type, length(evaluations) + 1L)
    }
    metrics <- do.call(rbind, lapply(evaluations, `[[`, "metrics"))
  } else if (method %in% c("edge_block", "node_block")) {
    topology <- quicknet_sym_topology(ctx, method, candidates, edges)
    metrics <- topology$metrics
    extra$blocked_adjacency <- topology$matrices
  } else if (method == "combination") {
    pairs <- utils::combn(candidates, 2, simplify = FALSE)
    metrics <- do.call(rbind, lapply(pairs, function(s) quicknet_sym_pair(ctx, s[1], s[2])))
    metrics$target <- paste(metrics$target_a, metrics$target_b, sep = "+")
    for (s in pairs) evaluations[[length(evaluations) + 1L]] <- quicknet_sym_evaluate(ctx, s, 1, "state_combination", length(evaluations) + 1L)
    extra$pair_scores <- metrics
  } else if (method == "sequence") {
    # Use the same scored candidate table as the full Python analyzer, including
    # its initial ordering when efficacy values tie at the pool boundary.
    base <- quicknet_sym_scores(ctx, candidates)$target_scores
    seq <- quicknet_sym_sequence(ctx, base)
    metrics <- seq$table
    extra$sequence <- seq$table
    extra$sequence_paths <- seq$paths
  } else if (method == "symperturb") {
    scores <- quicknet_sym_scores(ctx, candidates)
    robustness <- quicknet_sym_robustness(ctx, candidates)
    bootstrap <- quicknet_sym_bootstrap(ctx, candidates, bootstrap_indices)
    seq <- quicknet_sym_sequence(ctx, scores$target_scores)
    metrics <- scores$target_scores
    extra <- c(scores, list(robustness = robustness$summary, scenario_ranks = robustness$ranks,
                            bootstrap = bootstrap$summary, bootstrap_draws = bootstrap$draws,
                            bootstrap_indices = bootstrap$indices,
                            sequence = seq$table, sequence_paths = seq$paths))
    for (t in candidates) evaluations[[length(evaluations) + 1L]] <- quicknet_sym_evaluate(ctx, t, 1, "state_vKO", length(evaluations) + 1L)
  }
  if (method != "symperturb" && !is.null(bootstrap_indices)) stop("bootstrap_indices is only used by method = 'symperturb'.", call. = FALSE)
  perturbations <- if (length(evaluations)) do.call(rbind, lapply(evaluations, `[[`, "nodes")) else data.frame()
  rankings <- quicknet_perturb_rank(metrics)
  obj <- quicknet_perturbation_object(method, fit$model,
    settings = c(cfg, list(anchors = ctx$anchors, symptom_weights = ctx$weights, modules = ctx$modules,
                          costs = ctx$costs, candidate_targets = candidates,
                          interpretation = "model-implied virtual perturbation; not a causal intervention effect")),
    baseline = list(mean = ctx$network$mu, covariance = ctx$network$covariance, observed_mean = ctx$observed),
    perturbations = perturbations, metrics = metrics, rankings = rankings)
  obj$network <- ctx$network
  obj$network_edges <- quicknet_sym_edges(ctx)
  obj$metadata <- quicknet_sym_metadata(ctx, candidates)
  obj$moments <- lapply(evaluations, `[[`, "moments")
  for (nm in names(extra)) obj[[nm]] <- extra[[nm]]
  obj$report <- paste0("SymPerturb ", method, " using original-data Gaussian moments (ridge = ", cfg$ridge,
    ", topology edge threshold = ", cfg$edge_threshold, ", state map = ", cfg$state_map, "). ",
    "The intervention network is re-estimated from fit$data independently of the original fit's graph. ",
    "State benefits are baseline-SD standardized and evaluated on non-target outcomes. ",
    if (method == "symperturb") "VPPS uses seven utilities within the candidate set; robustness is reported separately. " else "",
    if (method == "combination") "Pair value is joint benefit minus the better single-target benefit on the same non-target set. " else "",
    if (method %in% c("edge_block", "node_block")) "Communication block is relative finite-step adjacency propagation loss. " else "",
    if (method == "sequence") "Sequence objectives use discounted marginal benefits and costs with beam search. " else "",
    "Results are model-implied in silico simulations, not causal intervention effects.\n",
    "Reference: Zhu, Z., Yu, J., Hu, T., Yang, Z., & Wang, J. (2026). SymPerturb converts symptom-network structure into testable intervention priorities. arXiv:2607.28673v1. Revised method specification and SymPerturb 0.1.0.")
  obj
}
