quicknet_nira_make_psock_cluster <- function(worker_count) {
  thread_variables <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "BLIS_NUM_THREADS"
  )
  previous_values <- Sys.getenv(thread_variables, unset = NA_character_)
  on.exit({
    previously_set <- !is.na(previous_values)
    if (any(previously_set)) {
      do.call(
        Sys.setenv,
        as.list(previous_values[previously_set])
      )
    }
    if (any(!previously_set)) {
      Sys.unsetenv(thread_variables[!previously_set])
    }
  }, add = TRUE)

  worker_values <- stats::setNames(
    rep("1", length(thread_variables)),
    thread_variables
  )
  do.call(Sys.setenv, as.list(worker_values))
  parallel::makeCluster(worker_count, type = "PSOCK")
}

quicknet_nira_make_streams <- function(seed, n) {
  if (!quicknet_nira_is_whole_number(seed) || seed < 0 ||
      seed > .Machine$integer.max) {
    stop("seed must be a non-negative integer no larger than .Machine$integer.max.", call. = FALSE)
  }
  if (!quicknet_nira_is_integer_count(n)) {
    stop("n must be a positive integer when constructing RNG streams.", call. = FALSE)
  }

  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed))
  streams <- vector("list", n)
  streams[[1L]] <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (n > 1L) {
    for (i in 2:n) {
      streams[[i]] <- parallel::nextRNGStream(streams[[i - 1L]])
    }
  }
  streams
}

quicknet_nira_expand_stream <- function(stream, n) {
  if (!is.numeric(stream) || length(stream) < 2L) {
    stop("stream must be a valid L'Ecuyer-CMRG RNG state.", call. = FALSE)
  }
  if (!quicknet_nira_is_integer_count(n)) {
    stop("n must be a positive integer when expanding an RNG stream.", call. = FALSE)
  }
  streams <- vector("list", n)
  streams[[1L]] <- stream
  if (n > 1L) {
    for (i in 2:n) {
      streams[[i]] <- parallel::nextRNGSubStream(streams[[i - 1L]])
    }
  }
  streams
}

quicknet_nira_with_stream <- function(stream, code) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  assign(".Random.seed", stream, envir = .GlobalEnv)
  force(code)
}

quicknet_nira_simulate_condition <- function(weight_matrix,
                                              thresholds,
                                              n_samples,
                                              engine,
                                              stream,
                                              beta = 1,
                                              n_iter = 100L) {
  quicknet_nira_with_stream(stream, {
    if (engine == "literature") {
      samples <- IsingSampler::IsingSampler(
        n = n_samples,
        graph = weight_matrix,
        thresholds = thresholds,
        beta = beta,
        nIter = n_iter,
        responses = c(0L, 1L),
        method = "MH"
      )
    } else {
      samples <- quicknet_nira_native_samples(
        weight_matrix = weight_matrix,
        thresholds = thresholds,
        n_samples = n_samples,
        beta = beta,
        n_iter = n_iter
      )
    }

    samples <- as.matrix(samples)
    storage.mode(samples) <- "integer"
    if (!identical(dim(samples), c(as.integer(n_samples), length(thresholds)))) {
      stop("The simulation engine returned an unexpected sample dimension.", call. = FALSE)
    }
    if (anyNA(samples) || any(!samples %in% c(0L, 1L))) {
      stop("The simulation engine returned values outside the required 0/1 support.", call. = FALSE)
    }
    colnames(samples) <- names(thresholds)
    samples
  })
}

quicknet_nira_native_samples <- function(weight_matrix,
                                          thresholds,
                                          n_samples,
                                          beta = 1,
                                          n_iter = 100L) {
  p <- length(thresholds)
  states <- matrix(
    stats::rbinom(n_samples * p, size = 1L, prob = 0.5),
    nrow = n_samples,
    ncol = p
  )
  for (iteration in seq_len(n_iter)) {
    for (node_index in seq_len(p)) {
      linear_predictor <- beta * (
        thresholds[[node_index]] +
          drop(states %*% weight_matrix[node_index, ])
      )
      states[, node_index] <- stats::rbinom(
        n_samples,
        size = 1L,
        prob = stats::plogis(linear_predictor)
      )
    }
  }
  storage.mode(states) <- "integer"
  colnames(states) <- names(thresholds)
  states
}

quicknet_nira_score_statistics <- function(total_scores, condition, node = NA_character_) {
  total_scores <- as.numeric(total_scores)
  n <- length(total_scores)
  score_mean <- mean(total_scores)
  score_sd <- stats::sd(total_scores)
  score_se <- score_sd / sqrt(n)
  data.frame(
    condition = condition,
    node = node,
    n = n,
    mean_total_score = score_mean,
    sd_total_score = score_sd,
    se_total_score = score_se,
    ci_lower = score_mean - stats::qnorm(0.975) * score_se,
    ci_upper = score_mean + stats::qnorm(0.975) * score_se,
    stringsAsFactors = FALSE
  )
}

quicknet_nira_pooled_sd <- function(original_scores, intervention_scores) {
  n_original <- length(original_scores)
  n_intervention <- length(intervention_scores)
  if (n_original < 2L || n_intervention < 2L) return(NA_real_)
  original_sd <- stats::sd(original_scores)
  intervention_sd <- stats::sd(intervention_scores)
  denominator_df <- n_original + n_intervention - 2L
  pooled_variance <- (
    (n_original - 1L) * original_sd^2 +
      (n_intervention - 1L) * intervention_sd^2
  ) / denominator_df
  if (!is.finite(pooled_variance) || pooled_variance <= 0) return(NA_real_)
  sqrt(pooled_variance)
}

quicknet_nira_threshold_perturbation <- function(
    thresholds,
    perturbation_type,
    amount_of_SDs_perturbation) {
  threshold_delta <- amount_of_SDs_perturbation * stats::sd(thresholds)
  if (!is.finite(threshold_delta) || threshold_delta <= 0) {
    stop(
      paste(
        "The requested threshold delta must remain finite and strictly positive;",
        "adjust amount_of_SDs_perturbation."
      ),
      call. = FALSE
    )
  }
  direction <- if (identical(perturbation_type, "alleviating")) -1 else 1
  perturbed_thresholds <- thresholds + direction * threshold_delta
  if (any(!is.finite(perturbed_thresholds))) {
    stop(
      paste(
        "Perturbed thresholds must remain finite;",
        "reduce amount_of_SDs_perturbation."
      ),
      call. = FALSE
    )
  }
  if (any(perturbed_thresholds == thresholds)) {
    stop(
      paste(
        "The perturbation must change every target threshold at machine",
        "precision; increase amount_of_SDs_perturbation."
      ),
      call. = FALSE
    )
  }
  list(
    threshold_delta = threshold_delta,
    direction = direction
  )
}

quicknet_nira_run_conditions <- function(parameters,
                                         perturbation_type,
                                         amount_of_SDs_perturbation,
                                         n_samples,
                                         engine,
                                         stage_stream,
                                         store_samples,
                                         n_iter = 100L) {
  weight_matrix <- parameters$weight_matrix
  thresholds <- parameters$thresholds
  beta <- parameters$beta
  node_names <- names(thresholds)
  p <- length(thresholds)
  perturbation <- quicknet_nira_threshold_perturbation(
    thresholds = thresholds,
    perturbation_type = perturbation_type,
    amount_of_SDs_perturbation = amount_of_SDs_perturbation
  )
  threshold_delta <- perturbation$threshold_delta
  direction <- perturbation$direction
  streams <- quicknet_nira_expand_stream(stage_stream, p + 1L)

  original_samples <- quicknet_nira_simulate_condition(
    weight_matrix = weight_matrix,
    thresholds = thresholds,
    n_samples = n_samples,
    engine = engine,
    stream = streams[[1L]],
    beta = beta,
    n_iter = n_iter
  )
  original_scores <- rowSums(original_samples)
  baseline_statistics <- quicknet_nira_score_statistics(
    original_scores,
    condition = "original"
  )

  intervention_statistics <- vector("list", p)
  intervention_scores <- stats::setNames(vector("list", p), node_names)
  intervention_samples <- if (store_samples) {
    stats::setNames(vector("list", p), node_names)
  } else {
    NULL
  }
  intervention_thresholds <- stats::setNames(vector("list", p), node_names)

  for (node_index in seq_len(p)) {
    node <- node_names[[node_index]]
    changed_thresholds <- thresholds
    changed_thresholds[[node_index]] <- changed_thresholds[[node_index]] +
      direction * threshold_delta
    samples <- quicknet_nira_simulate_condition(
      weight_matrix = weight_matrix,
      thresholds = changed_thresholds,
      n_samples = n_samples,
      engine = engine,
      stream = streams[[node_index + 1L]],
      beta = beta,
      n_iter = n_iter
    )
    scores <- rowSums(samples)
    stats_row <- quicknet_nira_score_statistics(
      scores,
      condition = node,
      node = node
    )
    raw_difference <- stats_row$mean_total_score -
      baseline_statistics$mean_total_score
    directional_effect <- if (perturbation_type == "alleviating") {
      -raw_difference
    } else {
      raw_difference
    }
    pooled_sd <- quicknet_nira_pooled_sd(original_scores, scores)
    stats_row$original_mean <- baseline_statistics$mean_total_score
    stats_row$raw_mean_difference <- raw_difference
    stats_row$directional_effect <- directional_effect
    stats_row$absolute_mean_difference <- abs(raw_difference)
    stats_row$raw_cohen_d <- if (is.finite(pooled_sd)) raw_difference / pooled_sd else NA_real_
    stats_row$cohen_d <- if (is.finite(pooled_sd)) directional_effect / pooled_sd else NA_real_
    stats_row$pooled_sd <- pooled_sd
    stats_row$original_threshold <- thresholds[[node_index]]
    stats_row$intervened_threshold <- changed_thresholds[[node_index]]
    stats_row$threshold_change <- direction * threshold_delta
    stats_row$threshold_delta <- threshold_delta

    intervention_statistics[[node_index]] <- stats_row
    intervention_scores[[node_index]] <- scores
    intervention_thresholds[[node_index]] <- changed_thresholds
    if (store_samples) intervention_samples[[node_index]] <- samples
  }

  interventions <- do.call(rbind, intervention_statistics)
  rownames(interventions) <- NULL
  list(
    threshold_delta = threshold_delta,
    baseline = list(
      statistics = baseline_statistics,
      total_scores = original_scores,
      thresholds = thresholds
    ),
    interventions = interventions,
    intervention_scores = intervention_scores,
    intervention_thresholds = intervention_thresholds,
    samples = if (store_samples) {
      list(original = original_samples, interventions = intervention_samples)
    } else {
      NULL
    }
  )
}

quicknet_nira_permutation_one <- function(original_scores,
                                          intervention_scores,
                                          n_permutations,
                                          stream) {
  quicknet_nira_with_stream(stream, {
    original_scores <- as.numeric(original_scores)
    intervention_scores <- as.numeric(intervention_scores)
    n_original <- length(original_scores)
    n_intervention <- length(intervention_scores)
    pooled <- c(original_scores, intervention_scores)
    pooled_total <- sum(pooled)
    observed <- mean(intervention_scores) - mean(original_scores)
    permuted <- numeric(n_permutations)

    for (permutation_index in seq_len(n_permutations)) {
      intervention_index <- sample.int(
        length(pooled),
        size = n_intervention,
        replace = FALSE
      )
      intervention_sum <- sum(pooled[intervention_index])
      original_sum <- pooled_total - intervention_sum
      permuted[[permutation_index]] <-
        intervention_sum / n_intervention - original_sum / n_original
    }
    extreme_count <- sum(
      abs(permuted) + sqrt(.Machine$double.eps) >= abs(observed)
    )
    list(
      observed_raw_difference = observed,
      extreme_count = extreme_count,
      p_value = (extreme_count + 1) / (n_permutations + 1),
      n_permutations = n_permutations
    )
  })
}

quicknet_nira_run_permutation <- function(original_scores,
                                          intervention_scores,
                                          intervention_statistics,
                                          perturbation_type,
                                          n_permutations,
                                          p_adjust,
                                          stage_stream) {
  node_names <- names(intervention_scores)
  streams <- quicknet_nira_expand_stream(stage_stream, length(node_names))
  rows <- vector("list", length(node_names))
  for (node_index in seq_along(node_names)) {
    node <- node_names[[node_index]]
    permutation <- quicknet_nira_permutation_one(
      original_scores = original_scores,
      intervention_scores = intervention_scores[[node]],
      n_permutations = n_permutations,
      stream = streams[[node_index]]
    )
    stats_row <- intervention_statistics[
      intervention_statistics$node == node,
      ,
      drop = FALSE
    ]
    rows[[node_index]] <- data.frame(
      node = node,
      original_mean = stats_row$original_mean,
      intervention_mean = stats_row$mean_total_score,
      raw_mean_difference = stats_row$raw_mean_difference,
      directional_effect = stats_row$directional_effect,
      sd_total_score = stats_row$sd_total_score,
      se_total_score = stats_row$se_total_score,
      ci_lower = stats_row$ci_lower,
      ci_upper = stats_row$ci_upper,
      pooled_sd = stats_row$pooled_sd,
      raw_cohen_d = stats_row$raw_cohen_d,
      cohen_d = stats_row$cohen_d,
      extreme_count = permutation$extreme_count,
      p_value = permutation$p_value,
      n_permutations = permutation$n_permutations,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$p_adjust_method <- p_adjust
  out$p_adjusted <- stats::p.adjust(out$p_value, method = p_adjust)
  out$significant <- out$p_adjusted < 0.05
  rownames(out) <- NULL
  out
}

quicknet_nira_stability_worker <- function(task) {
  tryCatch(
    {
      old_kind <- RNGkind()
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
      RNGkind("L'Ecuyer-CMRG")

      simulate_one <- function(thresholds, stream) {
        assign(".Random.seed", stream, envir = .GlobalEnv)
        if (task$engine == "literature") {
          samples <- IsingSampler::IsingSampler(
            n = task$n_samples,
            graph = task$weight_matrix,
            thresholds = thresholds,
            beta = task$beta,
            nIter = task$n_iter,
            responses = c(0L, 1L),
            method = "MH"
          )
        } else {
          p <- length(thresholds)
          samples <- matrix(
            stats::rbinom(task$n_samples * p, size = 1L, prob = 0.5),
            nrow = task$n_samples,
            ncol = p
          )
          for (iteration in seq_len(task$n_iter)) {
            for (node_index in seq_len(p)) {
              eta <- task$beta * (
                thresholds[[node_index]] +
                  drop(samples %*% task$weight_matrix[node_index, ])
              )
              samples[, node_index] <- stats::rbinom(
                task$n_samples,
                size = 1L,
                prob = stats::plogis(eta)
              )
            }
          }
        }
        samples <- as.matrix(samples)
        if (!identical(dim(samples), c(as.integer(task$n_samples), length(thresholds))) ||
            anyNA(samples) || any(!samples %in% c(0L, 1L))) {
          stop("Invalid samples returned during a stability repetition.", call. = FALSE)
        }
        mean(rowSums(samples))
      }

      p <- length(task$thresholds)
      condition_means <- numeric(p + 1L)
      condition_means[[1L]] <- simulate_one(
        task$thresholds,
        task$streams[[1L]]
      )
      direction <- if (task$perturbation_type == "alleviating") -1 else 1
      for (node_index in seq_len(p)) {
        changed <- task$thresholds
        changed[[node_index]] <- changed[[node_index]] +
          direction * task$threshold_delta
        condition_means[[node_index + 1L]] <- simulate_one(
          changed,
          task$streams[[node_index + 1L]]
        )
      }
      absolute_differences <- abs(
        condition_means[-1L] - condition_means[[1L]]
      )
      ranks <- rank(-absolute_differences, ties.method = "first")
      list(
        ok = TRUE,
        task_index = task$task_index,
        condition_means = condition_means,
        absolute_differences = absolute_differences,
        ranks = as.integer(ranks),
        error = NA_character_
      )
    },
    error = function(error) {
      list(
        ok = FALSE,
        task_index = task$task_index,
        condition_means = NULL,
        absolute_differences = NULL,
        ranks = NULL,
        error = conditionMessage(error)
      )
    }
  )
}

quicknet_nira_run_stability <- function(parameters,
                                        perturbation_type,
                                        threshold_delta,
                                        n_samples,
                                        stability_reps,
                                        top_n,
                                        engine,
                                        stage_stream,
                                        use_parallel,
                                        ncores,
                                        n_iter = 100L) {
  p <- length(parameters$thresholds)
  stream_count <- as.double(stability_reps) * (p + 1L)
  if (stream_count > .Machine$integer.max) {
    stop(
      "stability_reps times the number of conditions exceeds the supported ",
      "integer range.",
      call. = FALSE
    )
  }
  all_streams <- quicknet_nira_expand_stream(
    stage_stream,
    as.integer(stream_count)
  )
  tasks <- vector("list", stability_reps)
  for (rep_index in seq_len(stability_reps)) {
    stream_start <- (rep_index - 1L) * (p + 1L) + 1L
    stream_end <- stream_start + p
    tasks[[rep_index]] <- list(
      task_index = rep_index,
      weight_matrix = parameters$weight_matrix,
      thresholds = parameters$thresholds,
      beta = parameters$beta,
      perturbation_type = perturbation_type,
      threshold_delta = threshold_delta,
      n_samples = n_samples,
      engine = engine,
      n_iter = n_iter,
      streams = all_streams[stream_start:stream_end]
    )
  }

  if (use_parallel && stability_reps > 1L && ncores > 1L) {
    worker_count <- min(as.integer(ncores), as.integer(stability_reps))
    cluster <- quicknet_nira_make_psock_cluster(worker_count)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    results <- parallel::parLapply(
      cluster,
      tasks,
      quicknet_nira_stability_worker
    )
  } else {
    results <- lapply(tasks, quicknet_nira_stability_worker)
  }

  valid <- vapply(results, function(result) isTRUE(result$ok), logical(1))
  valid_reps <- sum(valid)
  failed_reps <- stability_reps - valid_reps
  failure_messages <- unique(vapply(
    results[!valid],
    function(result) result$error %||% "unknown worker error",
    character(1)
  ))
  failed_rep_indices <- vapply(
    results[!valid],
    function(result) as.integer(result$task_index),
    integer(1)
  )
  failure_records <- data.frame(
    repetition = failed_rep_indices,
    error = vapply(
      results[!valid],
      function(result) result$error %||% "unknown worker error",
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  if (valid_reps == 0L) {
    stop(
      "All Monte Carlo stability repetitions failed. ",
      paste(failure_messages, collapse = " | "),
      call. = FALSE
    )
  }
  failure_fraction <- failed_reps / stability_reps
  if (failure_fraction > 0.20) {
    stop(
      "More than 20% of Monte Carlo stability repetitions failed (",
      failed_reps, "/", stability_reps, "). ",
      paste(failure_messages, collapse = " | "),
      call. = FALSE
    )
  }
  if (failed_reps > 0L) {
    warning(
      failed_reps, " of ", stability_reps,
      " Monte Carlo stability repetitions failed: ",
      paste(failure_messages, collapse = " | "),
      call. = FALSE
    )
  }

  node_names <- names(parameters$thresholds)
  rank_matrix <- do.call(
    rbind,
    lapply(results[valid], function(result) result$ranks)
  )
  difference_matrix <- do.call(
    rbind,
    lapply(results[valid], function(result) result$absolute_differences)
  )
  condition_matrix <- matrix(
    NA_real_,
    nrow = stability_reps,
    ncol = p + 1L,
    dimnames = list(
      paste0("rep_", seq_len(stability_reps)),
      c("original", node_names)
    )
  )
  condition_matrix[valid, ] <- do.call(
    rbind,
    lapply(results[valid], function(result) result$condition_means)
  )
  colnames(rank_matrix) <- node_names
  colnames(difference_matrix) <- node_names
  rownames(rank_matrix) <- paste0("rep_", which(valid))
  rownames(difference_matrix) <- paste0("rep_", which(valid))

  rank_rows <- vector("list", p * p)
  row_index <- 0L
  for (node_index in seq_len(p)) {
    node_ranks <- rank_matrix[, node_index]
    for (rank_index in seq_len(p)) {
      row_index <- row_index + 1L
      count <- sum(node_ranks == rank_index)
      rank_rows[[row_index]] <- data.frame(
        node = node_names[[node_index]],
        rank = rank_index,
        count = count,
        proportion = count / valid_reps,
        cumulative_proportion = mean(node_ranks <= rank_index),
        stringsAsFactors = FALSE
      )
    }
  }
  rank_frequencies <- do.call(rbind, rank_rows)
  rownames(rank_frequencies) <- NULL

  top_k <- min(top_n, p)
  summary_rows <- vector("list", p)
  for (node_index in seq_len(p)) {
    ranks <- rank_matrix[, node_index]
    top1 <- mean(ranks == 1L)
    summary_rows[[node_index]] <- data.frame(
      node = node_names[[node_index]],
      top1_count = sum(ranks == 1L),
      top1_proportion = top1,
      top_k = top_k,
      top_k_count = sum(ranks <= top_k),
      top_k_cumulative_proportion = mean(ranks <= top_k),
      mean_rank = mean(ranks),
      median_rank = stats::median(ranks),
      rank_sd = stats::sd(ranks),
      rank_iqr = stats::IQR(ranks),
      mean_rank_mcse = stats::sd(ranks) / sqrt(valid_reps),
      top1_mcse = sqrt(top1 * (1 - top1) / valid_reps),
      mean_absolute_difference = mean(difference_matrix[, node_index]),
      sd_absolute_difference = stats::sd(difference_matrix[, node_index]),
      valid_reps = valid_reps,
      failed_reps = failed_reps,
      stringsAsFactors = FALSE
    )
  }
  node_summary <- do.call(rbind, summary_rows)
  rownames(node_summary) <- NULL

  list(
    method = "Monte Carlo simulation stability; not bootstrap stability",
    rank_frequencies = rank_frequencies,
    node_summary = node_summary,
    condition_means = condition_matrix,
    rank_matrix = rank_matrix,
    valid_reps = valid_reps,
    failed_reps = failed_reps,
    failed_rep_indices = failed_rep_indices,
    failure_records = failure_records,
    failure_messages = failure_messages
  )
}
