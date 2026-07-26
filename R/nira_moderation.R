quicknet_nira_moderation_role_grid <- function(node_names) {
  p <- length(node_names)
  empty <- data.frame(
    role_id = character(),
    moderator_index = integer(),
    moderator = character(),
    moderated_node_1_index = integer(),
    moderated_node_1 = character(),
    moderated_node_2_index = integer(),
    moderated_node_2 = character(),
    stringsAsFactors = FALSE
  )
  if (p < 3L) return(empty)

  rows <- vector("list", p * choose(p - 1L, 2L))
  row_index <- 0L
  for (moderator_index in seq_len(p)) {
    edge_nodes <- setdiff(seq_len(p), moderator_index)
    edge_pairs <- utils::combn(edge_nodes, 2L)
    for (pair_index in seq_len(ncol(edge_pairs))) {
      row_index <- row_index + 1L
      node_1_index <- edge_pairs[1L, pair_index]
      node_2_index <- edge_pairs[2L, pair_index]
      rows[[row_index]] <- data.frame(
        role_id = paste(
          moderator_index,
          node_1_index,
          node_2_index,
          sep = ":"
        ),
        moderator_index = moderator_index,
        moderator = node_names[[moderator_index]],
        moderated_node_1_index = node_1_index,
        moderated_node_1 = node_names[[node_1_index]],
        moderated_node_2_index = node_2_index,
        moderated_node_2 = node_names[[node_2_index]],
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

quicknet_nira_empty_moderation_table <- function() {
  data.frame(
    role_id = character(),
    moderator = character(),
    moderated_node_1 = character(),
    moderated_node_2 = character(),
    full_sample_estimate = numeric(),
    mean_moderation_estimate = numeric(),
    estimate_scale = character(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    positive_proportion = numeric(),
    negative_proportion = numeric(),
    nonzero_proportion = numeric(),
    same_sign_proportion = numeric(),
    reference_sign = integer(),
    full_sample_sign_defined = logical(),
    direction_defined_reps = integer(),
    direction_undefined_reps = integer(),
    direction_defined_proportion = numeric(),
    stable_moderation = logical(),
    valid_reps = integer(),
    failed_reps = integer(),
    stringsAsFactors = FALSE
  )
}

quicknet_nira_mgm_interaction_values <- function(interactions,
                                                  expected_count) {
  weights_list <- interactions$weightsAgg
  if (is.null(weights_list) || length(weights_list) < 2L) {
    stop(
      "mgm did not return aggregated weights for selected moderation effects.",
      call. = FALSE
    )
  }
  raw_weights <- weights_list[[2L]]
  if (is.list(raw_weights)) {
    magnitudes <- vapply(
      raw_weights,
      function(weight) {
        if (length(weight) != 1L) {
          stop(
            "mgm returned a non-scalar aggregated moderation weight.",
            call. = FALSE
          )
        }
        as.numeric(weight)
      },
      numeric(1)
    )
  } else {
    magnitudes <- as.numeric(raw_weights)
  }
  if (length(magnitudes) != expected_count) {
    stop(
      "mgm moderation indicators and aggregated weights have inconsistent lengths.",
      call. = FALSE
    )
  }
  if (anyNA(magnitudes) || any(!is.finite(magnitudes)) ||
      any(magnitudes < 0)) {
    stop(
      "mgm returned an invalid aggregated moderation weight.",
      call. = FALSE
    )
  }

  signs_list <- interactions$signs
  if (is.null(signs_list) || length(signs_list) < 2L ||
      is.null(signs_list[[2L]])) {
    interaction_signs <- rep(NA_real_, expected_count)
  } else {
    raw_signs <- signs_list[[2L]]
    if (is.list(raw_signs)) {
      interaction_signs <- vapply(
        raw_signs,
        function(interaction_sign) {
          if (length(interaction_sign) != 1L) {
            stop(
              "mgm returned a non-scalar moderation sign.",
              call. = FALSE
            )
          }
          as.numeric(interaction_sign)
        },
        numeric(1)
      )
    } else {
      interaction_signs <- as.numeric(raw_signs)
    }
    if (length(interaction_signs) != expected_count) {
      stop(
        "mgm moderation indicators and signs have inconsistent lengths.",
        call. = FALSE
      )
    }
    invalid_sign <- !is.na(interaction_signs) &
      (!is.finite(interaction_signs) |
        !interaction_signs %in% c(-1, 0, 1))
    if (any(invalid_sign)) {
      stop(
        "mgm returned a moderation sign outside -1, 0, 1, or NA.",
        call. = FALSE
      )
    }
  }

  magnitudes[magnitudes <= sqrt(.Machine$double.eps)] <- 0
  sign_defined <- magnitudes == 0 |
    (!is.na(interaction_signs) & interaction_signs %in% c(-1, 1))
  signed_estimates <- magnitudes * interaction_signs
  signed_estimates[magnitudes == 0] <- 0
  signed_estimates[!sign_defined] <- NA_real_
  list(
    magnitudes = magnitudes,
    signed_estimates = signed_estimates,
    sign_defined = sign_defined
  )
}

quicknet_nira_moderation_worker <- function(task, context) {
  tryCatch(
    {
      old_kind <- RNGkind()
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) {
        old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      }
      on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
      RNGkind("L'Ecuyer-CMRG")
      assign(".Random.seed", task$stream, envir = .GlobalEnv)

      data <- context$data
      if (isTRUE(task$resample)) {
        case_index <- sample.int(
          nrow(data),
          size = nrow(data),
          replace = TRUE
        )
        data <- data[case_index, , drop = FALSE]
      }

      extract_one <- function(model, moderator_index) {
        role_rows <- context$role_grid$moderator_index == moderator_index
        role_ids <- context$role_grid$role_id[role_rows]
        magnitudes <- stats::setNames(
          numeric(sum(role_rows)),
          role_ids
        )
        signed_estimates <- stats::setNames(
          numeric(sum(role_rows)),
          role_ids
        )
        sign_defined <- stats::setNames(
          rep(TRUE, sum(role_rows)),
          role_ids
        )

        indicators <- model$interactions$indicator
        if (is.null(indicators) || length(indicators) < 2L ||
            is.null(indicators[[2L]]) || length(indicators[[2L]]) == 0L) {
          return(list(
            magnitudes = magnitudes,
            signed_estimates = signed_estimates,
            sign_defined = sign_defined
          ))
        }

        triples <- indicators[[2L]]
        if (is.null(dim(triples))) {
          if (length(triples) %% 3L != 0L) {
            stop(
              "mgm returned a malformed vector of three-way interactions.",
              call. = FALSE
            )
          }
          triples <- matrix(triples, ncol = 3L, byrow = TRUE)
        } else {
          triples <- as.matrix(triples)
        }
        if (ncol(triples) != 3L) {
          stop(
            "mgm returned a malformed three-way interaction matrix.",
            call. = FALSE
          )
        }
        if (anyNA(triples) || any(!is.finite(triples)) ||
            any(triples != round(triples)) ||
            any(triples < 1L | triples > context$p)) {
          stop(
            "mgm returned invalid node indices for a three-way interaction.",
            call. = FALSE
          )
        }
        triples <- matrix(
          as.integer(triples),
          nrow = nrow(triples),
          ncol = 3L
        )
        if (any(apply(triples, 1L, function(x) length(unique(x)) != 3L))) {
          stop(
            "mgm returned a three-way interaction with repeated nodes.",
            call. = FALSE
          )
        }

        interaction_values <- quicknet_nira_mgm_interaction_values(
          model$interactions,
          expected_count = nrow(triples)
        )

        for (interaction_index in seq_len(nrow(triples))) {
          triple <- triples[interaction_index, ]
          if (!moderator_index %in% triple) {
            stop(
              "mgm returned an interaction that does not include the requested moderator.",
              call. = FALSE
            )
          }
          edge_nodes <- sort(setdiff(triple, moderator_index))
          role_id <- paste(
            moderator_index,
            edge_nodes[[1L]],
            edge_nodes[[2L]],
            sep = ":"
          )
          if (!role_id %in% names(magnitudes)) {
            stop(
              "mgm returned an interaction outside the requested moderator role grid.",
              call. = FALSE
            )
          }
          moderation_magnitude <-
            interaction_values$magnitudes[[interaction_index]]
          if (magnitudes[[role_id]] != 0) {
            stop(
              "mgm returned a duplicate moderation role.",
              call. = FALSE
            )
          }
          magnitudes[[role_id]] <- moderation_magnitude

          signed_estimates[[role_id]] <-
            interaction_values$signed_estimates[[interaction_index]]
          sign_defined[[role_id]] <-
            interaction_values$sign_defined[[interaction_index]]
        }
        list(
          magnitudes = magnitudes,
          signed_estimates = signed_estimates,
          sign_defined = sign_defined
        )
      }

      magnitudes <- stats::setNames(
        numeric(nrow(context$role_grid)),
        context$role_grid$role_id
      )
      signed_estimates <- magnitudes
      sign_defined <- stats::setNames(
        rep(TRUE, nrow(context$role_grid)),
        context$role_grid$role_id
      )
      for (moderator_index in seq_len(context$p)) {
        # Fit each moderator separately so the same three-node interaction can
        # retain a distinct, paper-defined moderator role.
        model <- tryCatch(
          mgm::mgm(
            data = data,
            type = rep("c", context$p),
            level = rep(2L, context$p),
            moderators = moderator_index,
            lambdaSel = "EBIC",
            lambdaGam = context$lambda,
            ruleReg = context$rule,
            binarySign = TRUE,
            scale = FALSE,
            pbar = FALSE,
            warnings = FALSE,
            saveModels = FALSE,
            saveData = FALSE,
            # The literature-facing runMgmmAnalysis() uses mgm's standard
            # parameterization. For binary data this also preserves the
            # direction of the corresponding logit interaction.
            overparameterize = FALSE,
            signInfo = FALSE
          ),
          error = function(error) {
            stop(
              "candidate moderator '",
              context$node_names[[moderator_index]],
              "' failed: ",
              conditionMessage(error),
              call. = FALSE
            )
          }
        )
        role_estimates <- extract_one(model, moderator_index)
        role_ids <- names(role_estimates$magnitudes)
        magnitudes[role_ids] <- role_estimates$magnitudes
        signed_estimates[role_ids] <- role_estimates$signed_estimates
        sign_defined[role_ids] <- role_estimates$sign_defined
      }

      list(
        ok = TRUE,
        task_index = task$task_index,
        magnitudes = magnitudes,
        signed_estimates = signed_estimates,
        sign_defined = sign_defined,
        estimates = ifelse(
          sign_defined,
          signed_estimates,
          magnitudes
        ),
        error = NA_character_
      )
    },
    error = function(error) {
      list(
        ok = FALSE,
        task_index = task$task_index,
        magnitudes = NULL,
        signed_estimates = NULL,
        sign_defined = NULL,
        estimates = NULL,
        error = conditionMessage(error)
      )
    }
  )
}

quicknet_nira_validate_moderation_input <- function(data,
                                                     node_names,
                                                     rule,
                                                     lambda,
                                                     nboot,
                                                     stage_stream,
                                                     use_parallel,
                                                     ncores) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data frame for moderation testing.", call. = FALSE)
  }
  if (is.data.frame(data) &&
      !all(vapply(data, function(x) is.numeric(x) || is.integer(x), logical(1)))) {
    stop("All moderation variables must be numeric 0/1 variables.", call. = FALSE)
  }
  data <- as.matrix(data)
  if (!is.numeric(data) && !is.integer(data)) {
    stop("All moderation variables must be numeric 0/1 variables.", call. = FALSE)
  }
  if (nrow(data) < 2L || ncol(data) < 2L) {
    stop("Moderation testing requires at least two cases and two nodes.", call. = FALSE)
  }
  if (length(node_names) != ncol(data) ||
      is.null(colnames(data)) ||
      !identical(as.character(colnames(data)), as.character(node_names))) {
    stop(
      "data column names and node_names must be identical and in the same order; ",
      "moderation input is never silently reordered.",
      call. = FALSE
    )
  }
  if (anyNA(node_names) || any(node_names == "") || anyDuplicated(node_names)) {
    stop("node_names must be non-missing, non-empty, and unique.", call. = FALSE)
  }
  if (anyNA(data) || any(!is.finite(data)) || any(!data %in% c(0, 1))) {
    stop("Moderation data must contain only finite, non-missing 0/1 values.", call. = FALSE)
  }
  has_variation <- apply(data, 2L, function(x) length(unique(x)) == 2L)
  if (!all(has_variation)) {
    stop(
      "Every moderation variable must contain both 0 and 1. No variation in: ",
      paste(node_names[!has_variation], collapse = ", "),
      call. = FALSE
    )
  }

  rule <- toupper(as.character(rule))
  if (length(rule) != 1L || is.na(rule) || !rule %in% c("AND", "OR")) {
    stop("rule must be either 'AND' or 'OR'.", call. = FALSE)
  }
  if (!is.numeric(lambda) || length(lambda) != 1L ||
      is.na(lambda) || !is.finite(lambda) || lambda < 0 || lambda > 1) {
    stop("lambda must be one finite number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(nboot) || length(nboot) != 1L ||
      is.na(nboot) || !is.finite(nboot) ||
      nboot != round(nboot) || nboot < 1L ||
      nboot > .Machine$integer.max - 1) {
    stop(
      "nboot must be a positive integer no larger than .Machine$integer.max - 1.",
      call. = FALSE
    )
  }
  if (!is.numeric(stage_stream) || length(stage_stream) < 2L ||
      anyNA(stage_stream) || any(!is.finite(stage_stream))) {
    stop("stage_stream must be a valid L'Ecuyer-CMRG RNG state.", call. = FALSE)
  }
  if (!is.logical(use_parallel) || length(use_parallel) != 1L ||
      is.na(use_parallel)) {
    stop("use_parallel must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(ncores) || length(ncores) != 1L ||
      is.na(ncores) || !is.finite(ncores) ||
      ncores != round(ncores) || ncores < 1L ||
      ncores > .Machine$integer.max) {
    stop(
      "ncores must be a positive integer no larger than .Machine$integer.max.",
      call. = FALSE
    )
  }

  list(
    data = data,
    node_names = as.character(node_names),
    rule = rule,
    lambda = as.numeric(lambda),
    nboot = as.integer(nboot),
    stage_stream = stage_stream,
    use_parallel = use_parallel,
    ncores = as.integer(ncores)
  )
}

quicknet_nira_run_moderation <- function(data,
                                         node_names,
                                         rule,
                                         lambda,
                                         nboot,
                                         stage_stream,
                                         use_parallel,
                                         ncores) {
  validated <- quicknet_nira_validate_moderation_input(
    data = data,
    node_names = node_names,
    rule = rule,
    lambda = lambda,
    nboot = nboot,
    stage_stream = stage_stream,
    use_parallel = use_parallel,
    ncores = ncores
  )
  data <- validated$data
  node_names <- validated$node_names
  rule <- validated$rule
  lambda <- validated$lambda
  nboot <- validated$nboot
  use_parallel <- validated$use_parallel
  ncores <- validated$ncores

  role_grid <- quicknet_nira_moderation_role_grid(node_names)
  if (nrow(role_grid) == 0L) {
    return(list(
      table = quicknet_nira_empty_moderation_table(),
      stable_detected = FALSE,
      valid_reps = 0L,
      failed_reps = 0L,
      failed_rep_indices = integer(),
      failure_records = data.frame(
        repetition = integer(),
        error = character(),
        stringsAsFactors = FALSE
      ),
      failure_messages = character(),
      rule = rule,
      lambda = lambda,
      nboot = nboot,
      note = "Fewer than three nodes: no three-way moderation role can be tested."
    ))
  }

  context <- list(
    data = data,
    node_names = node_names,
    p = ncol(data),
    role_grid = role_grid,
    rule = rule,
    lambda = lambda
  )
  streams <- quicknet_nira_expand_stream(
    validated$stage_stream,
    nboot + 1L
  )
  full_sample_result <- quicknet_nira_moderation_worker(
    task = list(
      task_index = 0L,
      resample = FALSE,
      stream = streams[[1L]]
    ),
    context = context
  )
  if (!isTRUE(full_sample_result$ok)) {
    stop(
      "The full-sample moderated graphical model failed: ",
      full_sample_result$error,
      call. = FALSE
    )
  }

  tasks <- lapply(seq_len(nboot), function(bootstrap_index) {
    list(
      task_index = bootstrap_index,
      resample = TRUE,
      stream = streams[[bootstrap_index + 1L]]
    )
  })
  if (use_parallel && ncores > 1L && nboot > 1L) {
    worker_count <- min(ncores, nboot)
    cluster <- quicknet_nira_make_psock_cluster(worker_count)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    # Load MGM on one worker at a time. Some BLAS/OpenMP combinations can
    # stall when several fresh PSOCK workers initialize MGM concurrently.
    for (worker_index in seq_len(worker_count)) {
      worker_ready <- parallel::clusterCall(
        cluster[worker_index],
        function() requireNamespace("mgm", quietly = TRUE)
      )
      if (!identical(worker_ready, list(TRUE))) {
        stop(
          "The mgm package could not be loaded on PSOCK worker ",
          worker_index, ".",
          call. = FALSE
        )
      }
    }
    results <- parallel::parLapply(
      cluster,
      tasks,
      quicknet_nira_moderation_worker,
      context = context
    )
  } else {
    results <- lapply(
      tasks,
      quicknet_nira_moderation_worker,
      context = context
    )
  }

  valid <- vapply(results, function(result) isTRUE(result$ok), logical(1))
  valid_reps <- sum(valid)
  failed_reps <- nboot - valid_reps
  failure_messages <- unique(vapply(
    results[!valid],
    function(result) result$error,
    character(1)
  ))
  failure_messages <- failure_messages[
    !is.na(failure_messages) & nzchar(failure_messages)
  ]
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
      "All moderation case-resampling repetitions failed. ",
      paste(utils::head(failure_messages, 5L), collapse = " | "),
      call. = FALSE
    )
  }
  failure_fraction <- failed_reps / nboot
  if (failure_fraction > 0.20) {
    stop(
      "More than 20% of moderation case-resampling repetitions failed (",
      failed_reps, "/", nboot, "). ",
      paste(utils::head(failure_messages, 5L), collapse = " | "),
      call. = FALSE
    )
  }
  if (failed_reps > 0L) {
    warning(
      failed_reps, " of ", nboot,
      " moderation case-resampling repetitions failed: ",
      paste(utils::head(failure_messages, 5L), collapse = " | "),
      call. = FALSE
    )
  }

  bootstrap_magnitudes <- do.call(
    rbind,
    lapply(results[valid], function(result) result$magnitudes)
  )
  bootstrap_magnitudes <- as.matrix(bootstrap_magnitudes)
  colnames(bootstrap_magnitudes) <- role_grid$role_id
  bootstrap_signed_estimates <- do.call(
    rbind,
    lapply(results[valid], function(result) result$signed_estimates)
  )
  bootstrap_signed_estimates <- as.matrix(bootstrap_signed_estimates)
  colnames(bootstrap_signed_estimates) <- role_grid$role_id
  bootstrap_sign_defined <- do.call(
    rbind,
    lapply(results[valid], function(result) result$sign_defined)
  )
  bootstrap_sign_defined <- as.matrix(bootstrap_sign_defined)
  colnames(bootstrap_sign_defined) <- role_grid$role_id

  rows <- vector("list", nrow(role_grid))
  for (role_index in seq_len(nrow(role_grid))) {
    magnitude_estimates <- bootstrap_magnitudes[, role_index]
    signed_estimates <- bootstrap_signed_estimates[, role_index]
    direction_defined <- bootstrap_sign_defined[, role_index]
    full_sample_sign_defined <-
      full_sample_result$sign_defined[[role_index]]
    use_signed_scale <- isTRUE(full_sample_sign_defined) &&
      all(direction_defined)
    estimates <- if (use_signed_scale) {
      signed_estimates
    } else {
      magnitude_estimates
    }
    full_sample_estimate <- if (use_signed_scale) {
      full_sample_result$signed_estimates[[role_index]]
    } else {
      full_sample_result$magnitudes[[role_index]]
    }
    mean_estimate <- mean(estimates)
    interval <- as.numeric(stats::quantile(
      estimates,
      probs = c(0.025, 0.975),
      names = FALSE,
      type = 7
    ))
    positive_proportion <- if (use_signed_scale) {
      mean(estimates > 0)
    } else {
      NA_real_
    }
    negative_proportion <- if (use_signed_scale) {
      mean(estimates < 0)
    } else {
      NA_real_
    }
    reference_sign <- if (use_signed_scale) {
      sign(full_sample_estimate)
    } else {
      NA_integer_
    }
    if (use_signed_scale && reference_sign == 0L) {
      reference_sign <- sign(mean_estimate)
    }
    same_sign_proportion <- if (!use_signed_scale) {
      NA_real_
    } else if (reference_sign == 0L) {
      0
    } else {
      mean(sign(estimates) == reference_sign)
    }

    rows[[role_index]] <- data.frame(
      role_id = role_grid$role_id[[role_index]],
      moderator = role_grid$moderator[[role_index]],
      moderated_node_1 = role_grid$moderated_node_1[[role_index]],
      moderated_node_2 = role_grid$moderated_node_2[[role_index]],
      full_sample_estimate = full_sample_estimate,
      mean_moderation_estimate = mean_estimate,
      estimate_scale = if (use_signed_scale) "signed" else "magnitude",
      ci_lower = interval[[1L]],
      ci_upper = interval[[2L]],
      positive_proportion = positive_proportion,
      negative_proportion = negative_proportion,
      nonzero_proportion = mean(magnitude_estimates != 0),
      same_sign_proportion = same_sign_proportion,
      reference_sign = as.integer(reference_sign),
      full_sample_sign_defined = full_sample_sign_defined,
      direction_defined_reps = sum(direction_defined),
      direction_undefined_reps = sum(!direction_defined),
      direction_defined_proportion = mean(direction_defined),
      stable_moderation = interval[[1L]] > 0 || interval[[2L]] < 0,
      valid_reps = valid_reps,
      failed_reps = failed_reps,
      stringsAsFactors = FALSE
    )
  }
  moderation_table <- do.call(rbind, rows)
  rownames(moderation_table) <- NULL

  list(
    table = moderation_table,
    stable_detected = any(moderation_table$stable_moderation),
    valid_reps = valid_reps,
    failed_reps = failed_reps,
    failed_rep_indices = failed_rep_indices,
    failure_records = failure_records,
    failure_messages = failure_messages,
    rule = rule,
    lambda = lambda,
    nboot = nboot,
    method = paste(
      "mgm moderated graphical models with each node fitted as a distinct",
      "candidate moderator; case-resampling stability"
    ),
    estimate_scale = paste(
      "signed MGM aggregated interaction magnitude when MGM defines every",
      "selected sign for a role; otherwise magnitude-only with directional",
      "proportions reported as unavailable"
    ),
    stable_definition = "The 95% case-resampling interval excludes zero.",
    rng_kind = "L'Ecuyer-CMRG"
  )
}
