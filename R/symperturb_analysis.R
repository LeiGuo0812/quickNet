# Seven-utility prioritization and complete-pipeline uncertainty.

quicknet_sym_profile <- function(ctx, target) {
  cfg <- ctx$config
  other <- setdiff(ctx$nodes, target)
  doses <- cfg$dose_grid
  if (!any(doses == 1)) doses <- c(doses, 1)
  dr <- data.frame(target = target, alpha = doses,
                   system_benefit = vapply(doses, function(a) {
                     quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, target, a), other)
                   }, numeric(1)))
  full <- quicknet_sym_delta(ctx, target)
  eff_doses <- cfg$dose_efficiency_grid[cfg$dose_efficiency_grid > 0]
  dose_eff <- mean(vapply(eff_doses, function(a) {
    quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, target, a), other) / a
  }, numeric(1)))
  mods <- sort(setdiff(unique(ctx$modules), ctx$modules[[target]]))
  cross <- mean(vapply(mods, function(m) mean(full[ctx$modules == m]) >= cfg$module_threshold, logical(1)))
  base_q <- quicknet_sym_potential(ctx$network$adjacency, cfg)
  blocked <- quicknet_sym_block(ctx$network$adjacency, match(target, ctx$nodes), cfg$block_fraction)
  comm <- if (abs(base_q) < 1e-15) 0 else (base_q - quicknet_sym_potential(blocked, cfg)) / base_q
  eps <- cfg$responsiveness_epsilon
  response <- (quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, target, eps), other) -
                 quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, target, 0), other)) / eps
  row <- data.frame(target = target,
                    efficacy = quicknet_sym_benefit(ctx, full, other),
                    dose_efficiency = dose_eff,
                    breadth = mean(full[other] >= cfg$breadth_threshold),
                    cross_module = cross, communication_block = comm,
                    responsiveness = response, direct_target_benefit = full[[target]],
                    beneficial_spillover_mean = mean(pmax(full[other], 0)),
                    adverse_spillover_mean = mean(pmax(-full[other], 0)), row.names = NULL)
  list(row = row, dose_response = dr)
}

quicknet_sym_scores <- function(ctx, targets) {
  profiles <- lapply(targets, function(t) quicknet_sym_profile(ctx, t))
  base <- do.call(rbind, lapply(profiles, `[[`, "row"))
  doses <- do.call(rbind, lapply(profiles, `[[`, "dose_response"))
  ranking <- base$target[order(-base$efficacy)]
  k <- max(1, min(ctx$config$combination_partner_k, length(ranking) - 1L))
  cache <- new.env(parent = emptyenv())
  pairs <- list()
  values <- vapply(targets, function(target) {
    partners <- utils::head(setdiff(ranking, target), k)
    vals <- vapply(partners, function(partner) {
      pair <- sort(c(target, partner))
      # Index-based keys avoid collisions when symptom labels contain separators.
      key <- paste(match(pair, ctx$nodes), collapse = ":")
      if (!exists(key, cache, inherits = FALSE)) {
        row <- quicknet_sym_pair(ctx, pair[1], pair[2])
        assign(key, row, envir = cache)
        pairs[[length(pairs) + 1L]] <<- row
      }
      get(key, cache, inherits = FALSE)$incremental_pair_value
    }, numeric(1))
    if (ctx$config$combination_mode == "positive") vals <- pmax(vals, 0)
    mean(vals)
  }, numeric(1))
  base$combination_value <- unname(values)
  utilities <- quicknet_sym_utilities()
  weights <- quicknet_sym_named(ctx$config$vpps_weights, utilities, 1, "vpps_weights", TRUE)
  if (sum(weights) <= 1e-8) stop("VPPS weights must have positive total.", call. = FALSE)
  for (nm in utilities) base[[paste0(nm, "_norm")]] <- quicknet_sym_minmax(base[[nm]])
  base$vpps <- as.vector(as.matrix(base[paste0(utilities, "_norm")]) %*% weights / sum(weights))
  base$rank <- as.integer(rank(-base$vpps, ties.method = "min"))
  base <- base[order(base$rank, base$target), , drop = FALSE]
  doses <- doses[order(doses$target, doses$alpha), , drop = FALSE]
  rownames(base) <- rownames(doses) <- NULL
  list(target_scores = base, dose_response = doses, pair_scores = do.call(rbind, pairs))
}

quicknet_sym_set_benefit <- function(ctx, targets) {
  if (!length(targets)) return(0)
  quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, targets), setdiff(ctx$nodes, targets))
}

quicknet_sym_sequence <- function(ctx, base_scores) {
  cfg <- ctx$config
  if (cfg$sequence_length == 0) return(list(table = data.frame(), paths = list()))
  pool <- utils::head(base_scores$target[order(-base_scores$efficacy)], cfg$sequence_pool)
  L <- min(cfg$sequence_length, length(pool))
  beam <- list(list(nodes = character(), objective = 0, benefit = 0, path = data.frame()))
  for (step in seq_len(L)) {
    candidates <- list()
    for (previous in beam) {
      for (node in setdiff(pool, previous$nodes)) {
        selected <- c(previous$nodes, node)
        benefit <- quicknet_sym_set_benefit(ctx, selected)
        marginal <- benefit - previous$benefit
        objective <- previous$objective + cfg$sequence_eta^(step - 1) * marginal - cfg$sequence_cost_lambda * ctx$costs[[node]]
        path <- rbind(previous$path, data.frame(step = step, chosen_node = node,
                      cumulative_targets = paste(selected, collapse = " -> "),
                      system_benefit = benefit, marginal_benefit = marginal, objective = objective))
        candidates[[length(candidates) + 1L]] <- list(nodes = selected, objective = objective, benefit = benefit, path = path)
      }
    }
    idx <- order(-vapply(candidates, `[[`, numeric(1), "objective"))
    beam <- candidates[utils::head(idx, cfg$sequence_beam_width)]
  }
  tab <- do.call(rbind, lapply(seq_along(beam), function(i) {
    b <- beam[[i]]
    data.frame(sequence_rank = i, sequence = paste(b$nodes, collapse = " -> "),
               objective = b$objective, final_set_benefit = b$benefit, length = length(b$nodes))
  }))
  list(table = tab, paths = lapply(beam, `[[`, "path"))
}

quicknet_sym_refit_context <- function(ctx, data = ctx$data, update = list()) {
  cfg <- ctx$config
  for (nm in names(update)) cfg[nm] <- update[nm]
  cfg$run_robustness_scenarios <- FALSE
  cfg$bootstrap_replicates <- cfg$sequence_length <- 0L
  quicknet_sym_context(data, cfg, ctx$anchors, ctx$weights, ctx$modules, ctx$costs)
}

quicknet_sym_robustness <- function(ctx, targets) {
  if (!ctx$config$run_robustness_scenarios) return(list(summary = data.frame(), ranks = data.frame()))
  variants <- list(reference = list(), ridge_0 = list(ridge = 0), ridge_0.05 = list(ridge = .05),
                   breadth_0.05 = list(breadth_threshold = .05), breadth_0.15 = list(breadth_threshold = .15),
                   module_0.10 = list(module_threshold = .10), module_0.30 = list(module_threshold = .30),
                   gamma_0.35 = list(propagation_gamma = .35), gamma_0.55 = list(propagation_gamma = .55),
                   block_0.60 = list(block_fraction = .60), block_1.00 = list(block_fraction = 1),
                   partner_3 = list(combination_partner_k = 3), partner_8 = list(combination_partner_k = 8))
  ranks <- do.call(rbind, lapply(names(variants), function(label) {
    sub <- quicknet_sym_refit_context(ctx, update = variants[[label]])
    tab <- quicknet_sym_scores(sub, targets)$target_scores
    data.frame(scenario = label, target = tab$target, rank = tab$rank, vpps = tab$vpps)
  }))
  tab <- do.call(rbind, lapply(sort(targets), function(target) {
    r <- ranks$rank[ranks$target == target]
    data.frame(target = target, scenario_rank_sd = sqrt(mean((r - mean(r))^2)),
               best_scenario_rank = min(r), worst_scenario_rank = max(r))
  }))
  max_sd <- max(tab$scenario_rank_sd)
  tab$robustness_diagnostic <- if (max_sd > 0) 1 - tab$scenario_rank_sd / max_sd else 1
  tab <- tab[order(-tab$robustness_diagnostic), c("target", "scenario_rank_sd", "robustness_diagnostic", "best_scenario_rank", "worst_scenario_rank")]
  rownames(tab) <- NULL
  list(summary = tab, ranks = ranks)
}

quicknet_sym_bootstrap <- function(ctx, targets, indices = NULL) {
  B <- ctx$config$bootstrap_replicates
  n <- nrow(ctx$data)
  if (!is.null(indices)) {
    if (!is.matrix(indices) || !is.numeric(indices) || ncol(indices) != n || nrow(indices) != B ||
        any(!is.finite(indices)) || any(indices != floor(indices) | indices < 1 | indices > n)) {
      stop("bootstrap_indices must be a bootstrap_replicates by n matrix of one-based row indices.", call. = FALSE)
    }
  }
  if (B == 0L) return(list(summary = data.frame(), draws = data.frame(), indices = matrix(integer(), 0, n)))
  if (is.null(indices)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
      } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(ctx$config$random_seed)
    indices <- matrix(sample.int(n, n * B, replace = TRUE), nrow = B, byrow = TRUE)
  }
  top_k <- min(ctx$config$bootstrap_top_k, length(targets))
  draws <- do.call(rbind, lapply(seq_len(B), function(b) {
    sub <- quicknet_sym_refit_context(ctx, data = ctx$data[indices[b, ], , drop = FALSE])
    tab <- quicknet_sym_scores(sub, targets)$target_scores
    data.frame(replicate = b, target = tab$target, vpps = tab$vpps, rank = tab$rank, top_k = as.integer(tab$rank <= top_k))
  }))
  summary <- do.call(rbind, lapply(sort(targets), function(target) {
    g <- draws[draws$target == target, , drop = FALSE]
    qv <- stats::quantile(g$vpps, c(.025, .975), names = FALSE, type = 7)
    qr <- stats::quantile(g$rank, c(.025, .975), names = FALSE, type = 7)
    data.frame(target = target, vpps_median = stats::median(g$vpps), vpps_q025 = qv[1], vpps_q975 = qv[2],
               rank_median = stats::median(g$rank), rank_q025 = qr[1], rank_q975 = qr[2],
               top_k_selection_probability = mean(g$top_k), replicates = nrow(g))
  }))
  summary <- summary[order(summary$rank_median, summary$target), , drop = FALSE]
  rownames(summary) <- NULL
  list(summary = summary, draws = draws, indices = indices)
}

quicknet_sym_edges <- function(ctx) {
  W <- ctx$network$adjacency
  idx <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
  # Python traverses source indices before target indices.
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  data.frame(source = ctx$nodes[idx[, 1]], target = ctx$nodes[idx[, 2]],
             partial_correlation = ctx$network$partial_correlations[idx], thresholded_weight = W[idx])
}

quicknet_sym_metadata <- function(ctx, targets) {
  cfg <- ctx$config
  list(n = nrow(ctx$data), p = ncol(ctx$data), candidate_targets = length(targets),
       ridge = cfg$ridge, edge_threshold = cfg$edge_threshold, state_map = cfg$state_map,
       bounds = cfg$bounds, combination_mode = cfg$combination_mode,
       vpps_dimensions = quicknet_sym_utilities(), robustness_in_vpps = FALSE,
       used_pseudoinverse = ctx$network$used_pseudoinverse,
       spectral_radius_gamma_absW = quicknet_sym_radius(cfg$propagation_gamma * abs(ctx$network$adjacency)),
       causal_interpretation = FALSE, reference_version = "SymPerturb 0.1.0 (76dd417)",
       estimator = "sample covariance + ridge * diag(sample covariance)",
       bootstrap_rng = "R sample.int; use bootstrap_indices for identical cross-language resamples")
}
