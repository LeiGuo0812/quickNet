#' Estimate a cross-lagged panel network
#'
#' @param data Wide-format panel data.
#' @param nodes Base node names without wave suffix.
#' @param waves Wave identifiers used in column names.
#' @param id ID variable. If absent, row numbers are used as IDs.
#' @param prefix Separator between node and wave in wide columns.
#' @param standardize Backend standardization. NULL inherits TRUE in glmnet
#'   and \code{"none"} in psychonetrics; psychonetrics also accepts \code{"z"},
#'   \code{"quantile"} and \code{"z_per_wave"}.
#' @param standardize_data CLPN preprocessing: standardize each wave before
#'   pooling transitions? Defaults to FALSE, separate from glmnet standardization.
#' @param alpha Elastic-net mixing parameter passed to \code{glmnet}.
#' @param lambda_rule Lambda selection rule. One of \code{"lambda.1se"} or \code{"lambda.min"}.
#' @param nfolds Number of cross-validation folds.
#' @param seed Optional random seed for cross-validation folds. NULL uses the
#'   current R random-number state.
#' @param model Panel model. One of \code{"clpn"}, \code{"ri_clpm"},
#' \code{"panel_gvar"}, or \code{"panel_var"}.
#' @param ri_type Innovation structure used by \code{psychonetrics::ri_clpm()}.
#' @param stationary Optional stationarity constraints passed to
#' \code{psychonetrics::ri_clpm_stationary()}.
#' @param ... Named controls passed directly to cv.glmnet or the selected
#'   psychonetrics constructor. CLPN fixes Gaussian outcomes, pools adjacent
#'   waves and groups CV folds by participant; it reports the actual fold count.
#'
#' @return A \code{quicknet_fit} object. The edge matrix uses rows as outcomes/to nodes and columns as predictors/from nodes.
#' @export
PanelNet <- function(data,
                     nodes,
                     waves,
                     id = "id",
                     prefix = "_t",
                     standardize = NULL,
                     alpha = 1,
                     lambda_rule = c("lambda.1se", "lambda.min"),
                     nfolds = 10,
                     seed = NULL,
                     model = "clpn",
                     ri_type = c("cov", "chol", "prec", "ggm", "cor"),
                     stationary = NULL,
                     standardize_data = FALSE,
                     ...) {
  supplied <- names(match.call())
  model <- match.arg(model, c("clpn", "ri_clpm", "panel_gvar", "panel_var"))
  if (model != "clpn" && isTRUE(standardize_data)) stop("standardize_data applies only to CLPN.", call. = FALSE)
  standardize <- standardize %||% if (model == "clpn") TRUE else "none"
  lambda_rule <- match.arg(lambda_rule)
  ri_type <- match.arg(ri_type)
  if (model != "clpn" && any(c("alpha", "lambda_rule", "nfolds", "seed") %in% supplied)) {
    stop("alpha, lambda_rule, nfolds and seed apply only to the CLPN model.", call. = FALSE)
  }
  if (model != "ri_clpm" && (!is.null(stationary) || "ri_type" %in% supplied)) {
    stop("ri_type and stationary apply only to ri_clpm.", call. = FALSE)
  }
  quicknet_validate_input(
    data,
    model = model,
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix,
    missing = if (model == "clpn") "listwise" else list(...)$missing %||% "auto"
  )
  panel_data <- as.data.frame(data)
  if (!id %in% colnames(panel_data)) {
    panel_data[[id]] <- seq_len(nrow(panel_data))
  }
  if (model != "clpn") {
    return(quicknet_psychonetrics_panel_fit(
      data = panel_data,
      nodes = nodes,
      waves = waves,
      id = id,
      prefix = prefix,
      model = model,
      standardize = standardize,
      ri_type = ri_type,
      stationary = stationary,
      call = match.call(),
      ...
    ))
  }
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for PanelNet(model = 'clpn').", call. = FALSE)
  }
  design <- quicknet_clpn_design(
    panel_data = panel_data,
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix,
    standardize = standardize_data
  )
  fit <- quicknet_clpn_glmnet(
    predictors = design$predictors,
    outcomes = design$outcomes,
    alpha = alpha,
    lambda_rule = lambda_rule,
    nfolds = nfolds,
    seed = seed,
    groups = design$meta$id,
    backend_args = quicknet_merge_args(list(standardize = standardize), list(...))
  )
  mat <- fit$edge_matrix
  cross_lagged <- mat
  diag(cross_lagged) <- 0

  node_table <- quicknet_directed_node_table(mat)
  node_table <- merge(node_table, fit$predictability, by = "node", all.x = TRUE, sort = FALSE)
  edge_table <- quicknet_edge_table(mat, directed = TRUE, drop_zero = FALSE, include_diag = TRUE)
  edge_table$edge_type <- ifelse(edge_table$from == edge_table$to, "autoregressive", "cross_lagged")

  quicknet_fit(
    model = "clpn",
    data = panel_data,
    networks = list(default = mat, cross_lagged = cross_lagged),
    edges = edge_table,
    nodes = node_table,
    fit = list(design = design, glmnet = fit),
    meta = list(
      data_type = "panel",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      nodes = nodes,
      waves = waves,
      id = id,
      prefix = prefix,
      backend = "glmnet::cv.glmnet",
      standardize = standardize,
      standardize_data = standardize_data,
      alpha = alpha,
      lambda_rule = lambda_rule,
      nfolds = length(unique(fit$foldid)),
      requested_nfolds = nfolds,
      backend_args = list(...),
      backend_version = as.character(utils::packageVersion("glmnet")),
      backend_settings = fit$settings,
      method_presets = list(family = "gaussian", folds = "grouped by participant", design = "pooled adjacent waves"),
      analysis_sample = list(input_rows = nrow(panel_data),
        input_subjects = length(unique(panel_data[[id]])),
        complete_rows = length(design$retained_rows),
        analyzed_subjects = length(unique(design$meta$id)),
        temporal_rows = nrow(design$predictors),
        temporal_subjects = length(unique(design$meta$id)),
        temporal_rows_by_node = data.frame(node = nodes, observations = nrow(design$predictors)),
        dropped_rows = design$dropped_rows,
        counts_source = "complete-case CLPN design and participant-grouped folds"),
      seed = seed,
      call = match.call()
    )
  )
}

#' Estimate longitudinal ESM networks
#'
#' @param data Long-format intensive longitudinal data.
#' @param vars Variables to include as network nodes.
#' @param id ID variable.
#' @param day Optional day variable. NULL leaves it unspecified in the backend.
#' @param beep Optional measurement-occasion variable within day.
#' @param model Longitudinal model. One of \code{"graphicalVAR"},
#' \code{"mlVAR"}, or \code{"psychonetrics_gvar"}.
#' @param gamma EBIC hyperparameter in [0,1]. NULL selects 0.5 for
#'   graphicalVAR. Ignored for mlVAR and psychonetrics_gvar.
#' @param scale NULL inherits TRUE in graphicalVAR/mlVAR and no standardization
#'   in psychonetrics. A logical value explicitly selects scaling.
#' @param centerWithin NULL inherits TRUE in graphicalVAR and FALSE in
#'   psychonetrics. Older psychonetrics versions without this control support
#'   only FALSE or NULL. Not an mlVAR control; use its native scaleWithin if intended.
#' @param lags Positive integer vector of lags used by \code{mlVAR}.
#' @param estimator NULL inherits the mlVAR or psychonetrics estimator.
#' @param temporal NULL inherits the backend temporal structure. mlVAR resolves
#'   its default to correlated effects for at most six nodes, otherwise orthogonal.
#' @param contemporaneous mlVAR contemporaneous structure; NULL inherits its
#'   data-dependent default.
#' @param nCores Number of cores used by \code{mlVAR}.
#' @param ... Additional arguments passed to the selected backend.
#'
#' @return A \code{quicknet_fit} object with temporal and contemporaneous
#' networks. Backends that estimate a between-person layer also return
#' \code{between}; multi-lag mlVAR fits additionally return
#' \code{temporal_lag_*} layers. An unestimable mlVAR between-person layer is
#' omitted with a warning.
#' @export
LongitudinalNet <- function(data,
                            vars,
                            id = "id",
                            day = NULL,
                            beep = NULL,
                            model = "graphicalVAR",
                            gamma = NULL,
                            scale = NULL,
                            centerWithin = NULL,
                            lags = 1,
                            estimator = NULL,
                            temporal = NULL,
                            contemporaneous = NULL,
                            nCores = 1,
                            ...) {
  model <- match.arg(model, c("graphicalVAR", "mlVAR", "psychonetrics_gvar"))
  gamma <- quicknet_resolve_gamma(model, gamma)
  lags <- quicknet_validate_lags(lags)
  if (model == "graphicalVAR" && (!is.null(estimator) || !is.null(temporal) || !is.null(contemporaneous) || nCores != 1)) {
    stop("estimator, temporal, contemporaneous and nCores are not supported by graphicalVAR.", call. = FALSE)
  }
  if (model == "psychonetrics_gvar" && (!is.null(contemporaneous) || nCores != 1)) stop("psychonetrics_gvar fixes contemporaneous = 'ggm' and does not use nCores.", call. = FALSE)
  if (model != "mlVAR" && !identical(as.integer(lags), 1L)) stop("This backend supports only lag 1 in LongitudinalNet.", call. = FALSE)
  if (model == "mlVAR" && !is.null(centerWithin)) stop("centerWithin is not an mlVAR argument; use scaleWithin if intended.", call. = FALSE)
  scale <- scale %||% (model != "psychonetrics_gvar")
  if (model == "graphicalVAR") centerWithin <- centerWithin %||% TRUE
  if (model == "mlVAR") estimator <- estimator %||% "default"
  temporal_setting <- temporal %||% "default"
  contemporaneous_setting <- contemporaneous %||% "default"
  quicknet_validate_input(
    data,
    model = model,
    vars = vars,
    id = id,
    day = day,
    beep = beep
  )
  required <- c(vars, id, day, beep)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  index <- c(id, day, beep)
  input_order <- do.call(order, as.data.frame(data[, index, drop = FALSE]))
  dat <- data[input_order, , drop = FALSE]

  if (model == "psychonetrics_gvar") {
    if (!requireNamespace("psychonetrics", quietly = TRUE)) {
      stop("Package 'psychonetrics' is required for LongitudinalNet(model = 'psychonetrics_gvar').", call. = FALSE)
    }
    return(quicknet_psychonetrics_gvar_fit(
      data = dat,
      vars = vars,
      id = id,
      day = day,
      beep = beep,
      scale = scale,
      centerWithin = centerWithin,
      estimator = estimator,
      temporal = temporal,
      input_order = input_order,
      call = match.call(),
      ...
    ))
  }

  if (model == "graphicalVAR") {
    if (!requireNamespace("graphicalVAR", quietly = TRUE)) {
      stop("Package 'graphicalVAR' is required for LongitudinalNet(model = 'graphicalVAR').", call. = FALSE)
    }
    args <- quicknet_backend_args(list(...), graphicalVAR::mlGraphicalVAR,
      reserved = c("data", "vars", "idvar", "dayvar", "beepvar", "scale", "centerWithin", "gamma", "lags"),
      extra = names(formals(graphicalVAR::graphicalVAR)))
    args <- quicknet_merge_args(list(scale = scale, centerWithin = centerWithin, gamma = gamma, verbose = FALSE,
      subjectNetworks = quicknet_backend_default(graphicalVAR::mlGraphicalVAR, "subjectNetworks")), args)
    fit <- do.call(graphicalVAR::mlGraphicalVAR, c(list(data = dat, vars = vars, idvar = id),
      if (!is.null(day)) list(dayvar = day), if (!is.null(beep)) list(beepvar = beep), args))
    provenance <- quicknet_backend_provenance("graphicalVAR", "mlGraphicalVAR", list(...), args)
    temporal <- quicknet_from_qgraph_matrix(fit$fixedPDC, directed = TRUE)
    contemporaneous <- as.matrix(fit$fixedPCC)
    between <- as.matrix(fit$betweenNet)
    temporal_by_lag <- list()
  } else {
    if (!requireNamespace("mlVAR", quietly = TRUE)) {
      stop("Package 'mlVAR' is required for LongitudinalNet(model = 'mlVAR').", call. = FALSE)
    }
    args <- quicknet_backend_args(list(...), mlVAR::mlVAR,
      reserved = c("data", "vars", "idvar", "dayvar", "beepvar", "lags", "estimator", "temporal", "contemporaneous", "nCores", "scale"))
    args <- quicknet_merge_args(list(lags = lags, estimator = estimator, temporal = temporal_setting,
      contemporaneous = contemporaneous_setting, nCores = nCores, verbose = FALSE, scale = scale), args)
    fit <- do.call(mlVAR::mlVAR, c(list(data = dat, vars = vars, idvar = id),
      if (!is.null(day)) list(dayvar = day), if (!is.null(beep)) list(beepvar = beep), args))
    estimator <- fit$input$estimator
    temporal_setting <- fit$input$temporal
    if (contemporaneous_setting == "default") contemporaneous_setting <- if (length(vars) > 6) "orthogonal" else "correlated"
    settings <- quicknet_merge_args(args, list(estimator = estimator, temporal = temporal_setting, contemporaneous = contemporaneous_setting))
    provenance <- quicknet_backend_provenance("mlVAR", "mlVAR", list(...), settings)
    temporal_by_lag <- quicknet_mlvar_temporal_networks(fit, vars, lags)
    temporal <- temporal_by_lag[[1]]
    contemporaneous <- quicknet_mlvar_get_net(fit, "contemporaneous", vars)
    between <- quicknet_mlvar_get_net(fit, "between", vars)
  }
  colnames(temporal) <- rownames(temporal) <- vars
  colnames(contemporaneous) <- rownames(contemporaneous) <- vars
  if (!is.null(between)) colnames(between) <- rownames(between) <- vars

  networks <- list(default = temporal, temporal = temporal, contemporaneous = contemporaneous)
  if (!is.null(between)) networks$between <- between
  if (length(temporal_by_lag) > 1) {
    networks <- c(networks, temporal_by_lag)
  }
  reported_networks <- networks[setdiff(names(networks), "default")]
  edges <- quicknet_longitudinal_edges(reported_networks)
  nodes <- do.call(quicknet_bind_rows_fill, lapply(names(reported_networks), function(network_name) {
    if (quicknet_longitudinal_network_is_directed(network_name)) {
      quicknet_directed_node_table(reported_networks[[network_name]], network = network_name)
    } else {
      quicknet_node_table(reported_networks[[network_name]], network = network_name)
    }
  }))
  lag_info <- quicknet_longitudinal_lag_index(dat, vars, id, day, beep,
    if (model == "mlVAR") fit$input$compareToLags %||% lags else 1L, input_order)
  analysis_sample <- quicknet_longitudinal_sample(dat, vars, id, lag_info, fit, model)

  quicknet_fit(
    model = model,
    data = dat,
    networks = networks,
    edges = edges,
    nodes = nodes,
    fit = fit,
    meta = c(list(
      data_type = "intensive_longitudinal",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      vars = vars,
      id = id,
      day = day,
      beep = beep,
      gamma = gamma,
      scale = scale,
      centerWithin = if (model == "graphicalVAR") centerWithin else NULL,
      lags = if (model == "mlVAR") lags else NULL,
      estimator = if (model == "mlVAR") estimator else NULL,
      temporal = if (model == "mlVAR") temporal_setting else NULL,
      contemporaneous = if (model == "mlVAR") contemporaneous_setting else NULL,
      nCores = if (model == "mlVAR") nCores else NULL,
      analysis_sample = analysis_sample,
      lag_index = lag_info,
      input_order = input_order,
      call = match.call()
    ), provenance)
  )
}

#' Bootstrap stability for longitudinal network fits
#'
#' @param fit A \code{quicknet_fit} object returned by \code{PanelNet()} or \code{LongitudinalNet()}.
#' @param nboot Number of bootstrap samples.
#' @param seed Random seed.
#' @param nfolds Number of CV folds used when refitting CLPN.
#'
#' @details Resamples independent participants with replacement, keeping all
#'   their rows and time ordering together and giving each sampled copy a new ID.
#'   At least two participants are required. Intervals are 95% percentile
#'   intervals for off-diagonal edges, conditional on successful fits across all
#'   requested layers. Failed fits are not replaced; their causes and counts are
#'   recorded in the \code{resampling} attribute of the result and each table.
#'   A small number of participants or a regularized estimator may give poor
#'   interval coverage; these intervals do not constitute edge significance tests.
#' @return A named list of bootstrap stability tables.
#' @export
LongitudinalStability <- function(fit, nboot = 100, seed = 20260502, nfolds = NULL) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  if (!quicknet_is_positive_integer(nboot)) {
    stop("nboot must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("seed must be a finite number.", call. = FALSE)
  }
  if (!is.null(nfolds) && (!quicknet_is_positive_integer(nfolds) || nfolds < 3)) {
    stop("nfolds must be an integer of at least 3.", call. = FALSE)
  }
  supported_models <- c(
    "clpn", "ri_clpm", "panel_gvar", "panel_var",
    "graphicalVAR", "mlVAR", "psychonetrics_gvar"
  )
  if (!fit$model %in% supported_models) {
    stop(
      "LongitudinalStability() supports PanelNet() and LongitudinalNet() fits.",
      call. = FALSE
    )
  }
  failure_reason <- quicknet_fit_failure_reason(fit)
  if (!is.null(failure_reason)) stop("The original fit is not valid: ", failure_reason, call. = FALSE)
  quicknet_check_row_args(fit$meta$backend_args, "Longitudinal stability")
  if (fit$model != "clpn" && !is.null(nfolds)) stop("nfolds applies only to CLPN.", call. = FALSE)
  set.seed(seed)
  if (fit$model == "clpn") {
    return(quicknet_panel_bootstrap_stability(fit, nboot = nboot, seed = seed, nfolds = nfolds %||% fit$meta$nfolds))
  }
  if (fit$model %in% c("ri_clpm", "panel_gvar", "panel_var")) {
    return(quicknet_psychonetrics_panel_bootstrap_stability(fit, nboot = nboot, seed = seed))
  }
  quicknet_longitudinal_bootstrap_stability(fit, nboot = nboot, seed = seed)
}

quicknet_longitudinal_lag_index <- function(data, vars, id, day = NULL, beep = NULL,
                                            lags = 1L, input_order = seq_len(nrow(data))) {
  rows <- seq_len(nrow(data))
  day_values <- if (is.null(day)) rep(1L, nrow(data)) else data[[day]]
  # Integer codes avoid ambiguous concatenated identifiers such as a.b/c vs a/b.c.
  group <- interaction(match(data[[id]], unique(data[[id]])),
                       match(day_values, unique(day_values)), drop = TRUE)
  occasions <- if (is.null(beep)) stats::ave(rows, group, FUN = seq_along) else data[[beep]]
  blocks <- split(rows, group)
  complete <- stats::complete.cases(data[, vars, drop = FALSE])
  out <- lapply(lags, function(lag) {
    predecessor <- rep(NA_integer_, nrow(data))
    for (block in blocks) {
      matched <- match(occasions[block] - lag, occasions[block])
      predecessor[block] <- block[matched]
    }
    valid <- !is.na(predecessor)
    complete_pair <- complete & valid
    complete_pair[valid] <- complete_pair[valid] & complete[predecessor[valid]]
    data.frame(row = rows, input_row = input_order, subject = data[[id]],
      day = day_values, occasion = occasions, lag = lag, predecessor = predecessor,
      input_predecessor = input_order[predecessor], valid = valid,
      complete = complete_pair, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

quicknet_longitudinal_sample <- function(data, vars, id, lag_index, fit, model) {
  valid_rows <- tapply(lag_index$valid, lag_index$row, all)
  complete_rows <- tapply(lag_index$complete, lag_index$row, all)
  sample <- list(input_rows = nrow(data), input_subjects = length(unique(data[[id]])),
    complete_rows = sum(stats::complete.cases(data[, vars, drop = FALSE])),
    candidate_lag_rows = sum(valid_rows), complete_lag_rows = sum(complete_rows),
    complete_lag_subjects = length(unique(data[[id]][as.integer(names(complete_rows)[complete_rows])])) )
  if (model == "graphicalVAR") {
    sample$temporal_rows <- fit$fixedResults$N
    sample$temporal_subjects <- sample$complete_lag_subjects
    sample$counts_source <- "graphicalVAR fixedResults$N; complete lag pairs"
  } else if (model == "mlVAR") {
    node_models <- if (is.list(fit$output) && !is.null(fit$output$temporal)) {
      fit$output$temporal
    } else if (is.list(fit$output)) fit$output else list()
    counts <- vapply(node_models, function(x) tryCatch({
      if (is.list(x) && !inherits(x, c("lm", "merMod"))) {
        return(sum(vapply(x, stats::nobs, numeric(1))))
      }
      as.numeric(stats::nobs(x))
    }, error = function(e) NA_real_), numeric(1))
    if (length(counts) != length(vars)) counts <- stats::setNames(rep(NA_real_, length(vars)), vars)
    sample$temporal_rows_by_node <- data.frame(node = names(counts) %||% vars,
      observations = unname(counts), stringsAsFactors = FALSE)
    sample$temporal_rows <- if (length(unique(counts)) == 1L) unname(counts[[1L]]) else NA_real_
    subject_counts <- vapply(node_models, function(x) {
      tryCatch({
        if (is.list(x) && !inherits(x, c("lm", "merMod"))) {
          return(as.integer(sum(vapply(x, function(model) stats::nobs(model) > 0, logical(1)))))
        }
        frame <- stats::model.frame(x)
        if (!id %in% names(frame)) return(NA_integer_)
        length(unique(frame[[id]]))
      }, error = function(e) NA_integer_)
    }, integer(1))
    if (length(subject_counts) != length(vars)) subject_counts <- stats::setNames(rep(NA_integer_, length(vars)), vars)
    sample$temporal_subjects_by_node <- data.frame(node = names(subject_counts) %||% vars,
      subjects = unname(subject_counts), stringsAsFactors = FALSE)
    sample$temporal_subjects <- if (length(unique(subject_counts)) == 1L) unname(subject_counts[[1L]]) else NA_integer_
    sample$counts_source <- "stats::nobs and model.frame on mlVAR temporal models when available; unavailable counts remain NA"
  } else {
    sample$backend_nobs <- fit@sample@groups[, c("label", "nobs"), drop = FALSE]
    sample$temporal_rows <- sum(fit@sample@groups$nobs)
    # FIML can retain initial observations with unavailable lagged predictors.
    sample$temporal_subjects <- NA_integer_
    sample$counts_source <- "psychonetrics sample@groups$nobs; complete lag counts reported separately"
  }
  sample
}

quicknet_clpn_design <- function(panel_data,
                                 nodes,
                                 waves,
                                 id = "id",
                                 prefix = "_t",
                                 standardize = TRUE) {
  if (length(waves) < 2) {
    stop("At least two waves are required for CLPN.", call. = FALSE)
  }
  required_columns <- unlist(lapply(waves, function(wave) paste0(nodes, prefix, wave)))
  missing_columns <- setdiff(required_columns, colnames(panel_data))
  if (length(missing_columns) > 0) {
    stop("Missing panel columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (!id %in% colnames(panel_data)) {
    panel_data[[id]] <- seq_len(nrow(panel_data))
  }

  dat <- panel_data[, c(id, required_columns), drop = FALSE]
  retained_rows <- which(stats::complete.cases(dat))
  dat <- dat[retained_rows, , drop = FALSE]
  if (standardize) {
    dat[required_columns] <- lapply(dat[required_columns], function(x) as.numeric(scale(x)))
  }

  predictor_blocks <- list()
  outcome_blocks <- list()
  meta_blocks <- list()
  for (lag_index in seq_len(length(waves) - 1)) {
    from_wave <- waves[lag_index]
    to_wave <- waves[lag_index + 1]
    predictor_columns <- paste0(nodes, prefix, from_wave)
    outcome_columns <- paste0(nodes, prefix, to_wave)
    predictors <- as.matrix(dat[, predictor_columns, drop = FALSE])
    outcomes <- as.matrix(dat[, outcome_columns, drop = FALSE])
    colnames(predictors) <- nodes
    colnames(outcomes) <- nodes
    rownames(predictors) <- rownames(outcomes) <- paste0(dat[[id]], "_", from_wave, "_to_", to_wave)
    predictor_blocks[[length(predictor_blocks) + 1]] <- predictors
    outcome_blocks[[length(outcome_blocks) + 1]] <- outcomes
    meta_blocks[[length(meta_blocks) + 1]] <- data.frame(
      id = dat[[id]],
      input_row = retained_rows,
      from_wave = from_wave,
      to_wave = to_wave,
      row_id = rownames(predictors),
      stringsAsFactors = FALSE
    )
  }

  list(
    predictors = do.call(rbind, predictor_blocks),
    outcomes = do.call(rbind, outcome_blocks),
    meta = do.call(rbind, meta_blocks),
    retained_rows = retained_rows,
    dropped_rows = setdiff(seq_len(nrow(panel_data)), retained_rows),
    nodes = nodes,
    waves = waves
  )
}

quicknet_clpn_glmnet <- function(predictors,
                                 outcomes,
                                 alpha = 1,
                                 lambda_rule = c("lambda.1se", "lambda.min"),
                                 nfolds = 10,
                                 seed = NULL,
                                 groups = NULL,
                                 backend_args = list()) {
  if (!quicknet_is_positive_integer(nfolds) || nfolds < 3) stop("nfolds must be an integer of at least 3.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("alpha must be one finite number in [0, 1].", call. = FALSE)
  lambda_rule <- match.arg(lambda_rule)
  predictors <- as.matrix(predictors)
  outcomes <- as.matrix(outcomes)
  nodes <- colnames(outcomes)
  edge_matrix <- matrix(0, nrow = ncol(outcomes), ncol = ncol(predictors), dimnames = list(nodes, colnames(predictors)))
  predictability <- data.frame(node = nodes, cv_r_squared = NA_real_, lambda = NA_real_, nonzero_predictors = NA_integer_)
  fits <- list()

  if (!is.null(seed)) set.seed(seed)
  if (is.null(groups)) {
    nfolds <- max(3, min(nfolds, nrow(predictors)))
    foldid <- sample(rep(seq_len(nfolds), length.out = nrow(predictors)))
  } else {
    if (length(groups) != nrow(predictors)) {
      stop("groups must have one value per design row.", call. = FALSE)
    }
    unique_groups <- unique(groups)
    if (length(unique_groups) < 3) {
      stop("At least three independent subjects are required for grouped cross-validation.", call. = FALSE)
    }
    nfolds <- max(3, min(nfolds, length(unique_groups)))
    group_folds <- sample(rep(seq_len(nfolds), length.out = length(unique_groups)))
    foldid <- group_folds[match(groups, unique_groups)]
  }

  args <- quicknet_backend_args(backend_args, glmnet::cv.glmnet,
    reserved = c("x", "y", "alpha", "family", "nfolds", "foldid"),
    extra = names(formals(glmnet::glmnet)))
  if (!is.null(args$type.measure) && !args$type.measure %in% c("default", "mse")) {
    stop("CLPN cv_r_squared requires the Gaussian MSE criterion; type.measure must be 'mse' or 'default'.", call. = FALSE)
  }
  args <- quicknet_merge_args(list(alpha = alpha, family = "gaussian", nfolds = nfolds, foldid = foldid), args)
  for (target in nodes) {
    y <- outcomes[, target]
    cv_fit <- do.call(glmnet::cv.glmnet, c(list(x = predictors, y = y), args))
    lambda_value <- if (lambda_rule == "lambda.1se") cv_fit$lambda.1se else cv_fit$lambda.min
    coefficients <- as.matrix(stats::coef(cv_fit, s = lambda_value))
    edge_matrix[target, ] <- as.numeric(coefficients[colnames(predictors), 1])
    cv_mse <- cv_fit$cvm[which.min(abs(cv_fit$lambda - lambda_value))]
    null_mse <- mean((y - mean(y))^2)
    predictability[predictability$node == target, c("cv_r_squared", "lambda", "nonzero_predictors")] <- c(
      ifelse(is.finite(null_mse) && null_mse > 0, 1 - cv_mse / null_mse, NA_real_),
      lambda_value,
      sum(abs(edge_matrix[target, ]) > 1e-10)
    )
    fits[[target]] <- cv_fit
  }

  list(
    edge_matrix = edge_matrix,
    predictability = predictability,
    fits = fits,
    lambda_rule = lambda_rule,
    foldid = foldid,
    settings = args
  )
}

quicknet_psychonetrics_panel_fit <- function(data,
                                             nodes,
                                             waves,
                                             id,
                                             prefix,
                                             model,
                                             standardize,
                                             ri_type,
                                             stationary,
                                             call,
                                             ...) {
  if (!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("Package 'psychonetrics' is required for PanelNet(model = '", model, "').", call. = FALSE)
  }
  dat <- quicknet_panel_psychonetrics_data(
    data = data,
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix,
    standardize = FALSE
  )
  vars_matrix <- quicknet_panel_vars_matrix(nodes, waves, prefix)

  fun <- switch(model, ri_clpm = "ri_clpm", panel_gvar = "panelgvar", panel_var = "panelvar")
  args <- quicknet_psychonetrics_args(fun,
    c(list(data = dat$data, vars = vars_matrix, verbose = FALSE,
      standardize = if (is.character(standardize)) standardize else if (isTRUE(standardize)) "z_per_wave" else "none"),
      if (model == "ri_clpm") list(type = ri_type)), list(...))
  raw_model <- quicknet_capture_backend_warnings(do.call(get(fun, asNamespace("psychonetrics")), args))
  if (model == "ri_clpm" && !is.null(stationary)) {
    raw_model <- psychonetrics::ri_clpm_stationary(raw_model, stationary = stationary)
  }
  fit <- quicknet_psychonetrics_run(raw_model)

  networks <- if (model == "ri_clpm") {
    quicknet_ri_clpm_networks(fit, nodes = nodes, waves = waves, prefix = prefix)
  } else {
    temporal <- quicknet_psychonetrics_matrix(fit, "beta", nodes)
    within <- quicknet_psychonetrics_first_matrix(
      fit,
      c("omega_zeta_within", "sigma_zeta_within", "kappa_zeta_within"),
      nodes
    )
    between <- quicknet_psychonetrics_first_matrix(
      fit,
      c("omega_zeta_between", "sigma_zeta_between", "kappa_zeta_between"),
      nodes
    )
    list(default = temporal, temporal = temporal, within = within, between = between)
  }

  edges <- quicknet_longitudinal_edges(networks)
  node_tables <- lapply(names(networks), function(network_name) {
    if (quicknet_longitudinal_network_is_directed(network_name)) {
      quicknet_directed_node_table(networks[[network_name]], network = network_name)
    } else {
      quicknet_node_table(networks[[network_name]], network = network_name)
    }
  })

  quicknet_fit(
    model = model,
    data = dat$data,
    networks = networks,
    edges = edges,
    nodes = do.call(quicknet_bind_rows_fill, node_tables),
    fit = list(model = fit, fit_indices = quicknet_psychonetrics_fit_indices(fit)),
    meta = list(
      data_type = "panel",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = paste0("psychonetrics::", switch(model, ri_clpm = "ri_clpm", panel_gvar = "panelgvar", panel_var = "panelvar")),
      nodes = nodes,
      waves = waves,
      id = id,
      prefix = prefix,
      standardize = standardize,
      ri_type = if (model == "ri_clpm") ri_type else NULL,
      stationary = stationary,
      backend_args = list(...),
      backend_version = as.character(utils::packageVersion("psychonetrics")),
      backend_settings = quicknet_psychonetrics_settings(fit, args),
      estimator = fit@estimator,
      analysis_sample = list(input_rows = nrow(data), input_subjects = length(unique(data[[id]])),
        complete_rows = sum(stats::complete.cases(dat$data[, dat$required_columns, drop = FALSE])),
        analyzed_subjects = sum(fit@sample@groups$nobs),
        backend_nobs = fit@sample@groups[, c("label", "nobs"), drop = FALSE],
        counts_source = "psychonetrics sample@groups$nobs; one wide row per participant"),
      call = call
    )
  )
}

quicknet_psychonetrics_center_args <- function(value, fun = psychonetrics::var1) {
  if (!is.null(value) && (!is.logical(value) || length(value) != 1L || is.na(value))) {
    stop("centerWithin must be NULL or a single logical value.", call. = FALSE)
  }
  if (!"centerWithin" %in% names(formals(fun))) {
    if (isTRUE(value)) {
      stop("The installed psychonetrics version does not support centerWithin = TRUE; update psychonetrics or use FALSE/NULL.", call. = FALSE)
    }
    return(list())
  }
  list(centerWithin = value %||% quicknet_backend_default(fun, "centerWithin"))
}

quicknet_psychonetrics_gvar_fit <- function(data,
                                            vars,
                                            id,
                                            day,
                                            beep,
                                            scale,
                                            centerWithin,
                                            estimator,
                                            temporal,
                                            input_order = seq_len(nrow(data)),
                                            call,
                                            ...) {
  dots <- list(...)
  standardize <- dots$standardize %||% if (isTRUE(scale)) "z" else "none"
  dots$standardize <- NULL
  center_args <- quicknet_psychonetrics_center_args(centerWithin)
  centerWithin <- center_args$centerWithin %||% FALSE
  args <- quicknet_psychonetrics_args("gvar", c(list(data = data, vars = vars, idvar = id,
    standardize = standardize, verbose = FALSE), center_args,
    if (!is.null(day)) list(dayvar = day), if (!is.null(beep)) list(beepvar = beep),
    if (!is.null(estimator)) list(estimator = estimator), if (!is.null(temporal)) list(temporal = temporal)), dots)
  raw_model <- quicknet_capture_backend_warnings(do.call(psychonetrics::gvar, args))
  fit <- quicknet_psychonetrics_run(raw_model)
  temporal <- quicknet_psychonetrics_matrix(fit, "beta", vars)
  contemporaneous <- quicknet_psychonetrics_first_matrix(fit, c("omega_zeta", "sigma_zeta", "kappa_zeta"), vars)
  networks <- list(default = temporal, temporal = temporal, contemporaneous = contemporaneous)
  edges <- quicknet_longitudinal_edges(networks)
  nodes <- quicknet_bind_rows_fill(
    quicknet_directed_node_table(temporal, network = "temporal"),
    quicknet_node_table(contemporaneous, network = "contemporaneous")
  )
  lag_info <- quicknet_longitudinal_lag_index(data, vars, id, day, beep, 1L, input_order)

  quicknet_fit(
    model = "psychonetrics_gvar",
    data = data,
    networks = networks,
    edges = edges,
    nodes = nodes,
    fit = list(model = fit, fit_indices = quicknet_psychonetrics_fit_indices(fit)),
    meta = list(
      data_type = "intensive_longitudinal",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "psychonetrics::gvar",
      vars = vars,
      id = id,
      day = day,
      beep = beep,
      scale = scale,
      standardize = standardize,
      temporal = args$temporal,
      centerWithin = centerWithin,
      backend_version = as.character(utils::packageVersion("psychonetrics")),
      backend_settings = quicknet_psychonetrics_settings(fit, args),
      estimator = fit@estimator,
      backend_args = list(...),
      analysis_sample = quicknet_longitudinal_sample(data, vars, id, lag_info, fit, "psychonetrics_gvar"),
      lag_index = lag_info,
      input_order = input_order,
      call = call
    )
  )
}

quicknet_panel_psychonetrics_data <- function(data, nodes, waves, id, prefix, standardize) {
  dat <- as.data.frame(data)
  required_columns <- unlist(lapply(waves, function(wave) paste0(nodes, prefix, wave)))
  if (!id %in% colnames(dat)) dat[[id]] <- seq_len(nrow(dat))
  dat <- dat[, c(id, required_columns), drop = FALSE]
  if (isTRUE(standardize)) {
    dat[required_columns] <- lapply(dat[required_columns], function(x) as.numeric(scale(x)))
  }
  list(data = dat, required_columns = required_columns)
}

quicknet_panel_vars_matrix <- function(nodes, waves, prefix) {
  vars_matrix <- outer(nodes, waves, Vectorize(function(node, wave) paste0(node, prefix, wave)))
  rownames(vars_matrix) <- nodes
  colnames(vars_matrix) <- as.character(waves)
  vars_matrix
}

quicknet_psychonetrics_run <- function(model) {
  construction_warnings <- attr(model, "quicknet_backend_warnings")
  invisible(utils::capture.output({
    out <- quicknet_capture_backend_warnings(suppressMessages(psychonetrics::runmodel(model)))
  }))
  recorded <- unique(c(construction_warnings, attr(out, "quicknet_backend_warnings")))
  if (length(recorded)) attr(out, "quicknet_backend_warnings") <- recorded
  out
}

quicknet_psychonetrics_fit_indices <- function(fit) {
  out <- NULL
  invisible(utils::capture.output({
    out <- tryCatch(psychonetrics::fit(fit), error = function(e) NULL)
  }))
  if (is.null(out)) return(data.frame())
  as.data.frame(out)
}

quicknet_psychonetrics_matrix <- function(fit, matrix_name, vars) {
  mat <- tryCatch(psychonetrics::getmatrix(fit, matrix_name), error = function(e) {
    stop("Could not extract psychonetrics matrix '", matrix_name, "': ", conditionMessage(e), call. = FALSE)
  })
  if (is.list(mat) || is.null(mat)) {
    stop("psychonetrics matrix '", matrix_name,
      "' must be one numeric matrix; multiple-group matrix lists are not supported by this network layer.", call. = FALSE)
  }
  mat <- as.matrix(mat)
  if (!is.numeric(mat) || !identical(dim(mat), rep(as.integer(length(vars)), 2L)) ||
      any(!is.finite(mat))) {
    stop("psychonetrics matrix '", matrix_name, "' has unexpected dimensions or non-finite values; expected ",
      length(vars), " by ", length(vars), ".", call. = FALSE)
  }
  if (!is.null(rownames(mat)) && !is.null(colnames(mat)) &&
      setequal(rownames(mat), vars) && setequal(colnames(mat), vars)) {
    mat <- mat[vars, vars, drop = FALSE]
  }
  colnames(mat) <- rownames(mat) <- vars
  mat
}

quicknet_psychonetrics_first_matrix <- function(fit, candidates, vars) {
  available <- tryCatch(fit@matrices$name, error = function(e) character())
  computed <- tryCatch(names(fit@modelmatrices[[1]]), error = function(e) character())
  # A Cholesky parameterization only models lowertri_* directly. Prefer its
  # derived covariance when none of the requested matrices is directly modeled.
  ordered <- c(candidates[candidates %in% available],
               candidates[grepl("^sigma", candidates)], candidates)
  for (candidate in unique(ordered)) {
    if (candidate %in% c(available, computed)) {
      mat <- quicknet_psychonetrics_matrix(fit, candidate, vars)
      diag(mat) <- 0
      return(mat)
    }
  }
  stop("No supported psychonetrics network matrix was available among: ",
    paste(candidates, collapse = ", "), ".", call. = FALSE)
}

quicknet_ri_clpm_networks <- function(fit, nodes, waves, prefix) {
  # RI-CLPM stores wave-specific innovations first, followed by random
  # intercepts. Read full matrices so covariance/precision/Cholesky models
  # and fixed parameters are represented as well as GGM parameters.
  p <- length(nodes)
  expected_dim <- p * (length(waves) + 1L)
  beta <- as.matrix(psychonetrics::getmatrix(fit, "beta"))
  available <- fit@matrices$name
  innovation_name <- c("omega_zeta", "sigma_zeta", "kappa_zeta")
  innovation_name <- innovation_name[innovation_name %in% available]
  if (!length(innovation_name)) innovation_name <- "sigma_zeta"
  innovations <- as.matrix(psychonetrics::getmatrix(fit, innovation_name[[1]]))
  if (!all(dim(beta) == c(expected_dim, expected_dim)) ||
      !all(dim(innovations) == c(expected_dim, expected_dim))) {
    stop("RI-CLPM returned matrices with unexpected dimensions.", call. = FALSE)
  }
  temporal <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  contemporaneous <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  random_intercept <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))

  for (from_index in seq_along(nodes)) {
    for (to_index in seq_along(nodes)) {
      values <- numeric()
      for (wave_index in seq_len(length(waves) - 1)) {
        values <- c(values, beta[wave_index * p + to_index,
                                  (wave_index - 1L) * p + from_index])
      }
      temporal[to_index, from_index] <- quicknet_safe_mean(values)
    }
  }

  for (i in seq_along(nodes)) {
    node_i <- nodes[[i]]
    for (j in seq_along(nodes)) {
      node_j <- nodes[[j]]
      if (node_i == node_j) {
        contemporaneous[node_i, node_j] <- 0
        random_intercept[node_i, node_j] <- 0
        next
      }
      innovation_values <- numeric()
      for (wave_index in seq_along(waves)) {
        offset <- (wave_index - 1L) * p
        innovation_values <- c(innovation_values, innovations[offset + i, offset + j])
      }
      contemporaneous[node_i, node_j] <- quicknet_safe_mean(innovation_values)
      offset <- length(waves) * p
      random_intercept[node_i, node_j] <- innovations[offset + i, offset + j]
    }
  }

  cross_lagged <- temporal
  diag(cross_lagged) <- 0
  list(
    default = temporal,
    temporal = temporal,
    cross_lagged = cross_lagged,
    contemporaneous = contemporaneous,
    random_intercept = random_intercept
  )
}

quicknet_longitudinal_edges <- function(networks) {
  rows <- lapply(names(networks), function(network_name) {
    directed <- quicknet_longitudinal_network_is_directed(network_name)
    edge_table <- quicknet_edge_table(
      networks[[network_name]],
      network = network_name,
      directed = directed,
      drop_zero = FALSE,
      include_diag = directed
    )
    if (directed) {
      edge_table$edge_type <- ifelse(edge_table$from == edge_table$to, "autoregressive", "cross_lagged")
    }
    edge_table
  })
  do.call(quicknet_bind_rows_fill, rows)
}

quicknet_longitudinal_network_is_directed <- function(network_name) {
  network_name %in% c("default", "temporal", "cross_lagged") ||
    grepl("^(temporal_)?lag_", network_name)
}

quicknet_mlvar_get_net <- function(fit, type, vars, lag = NULL) {
  args <- list(fit, type = type, nonsig = "show")
  if (!is.null(lag) && identical(type, "temporal")) args$lag <- lag
  result <- tryCatch(do.call(mlVAR::getNet, args), error = function(e) {
    if (identical(type, "between")) {
      warning("mlVAR could not estimate the between-person network; this layer is omitted: ",
              conditionMessage(e), call. = FALSE)
      return(NULL)
    }
    stop("Could not extract the mlVAR ", type, " network: ", conditionMessage(e), call. = FALSE)
  })
  if (is.null(result) && identical(type, "between")) return(NULL)
  quicknet_mlvar_standardize_net(result, type = type, vars = vars)
}

quicknet_mlvar_temporal_networks <- function(fit,
                                             vars,
                                             lags,
                                             get_net = quicknet_mlvar_get_net) {
  lag_values <- as.integer(lags)
  stats::setNames(
    lapply(seq_along(lag_values), function(lag_index) {
      get_net(fit, "temporal", vars, lag = lag_index)
    }),
    paste0("temporal_lag_", lag_values)
  )
}

quicknet_mlvar_standardize_net <- function(result, type, vars) {
  if (is.null(result)) {
    mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  } else if (is.matrix(result)) {
    mat <- result
  } else if (is.array(result)) {
    mat <- result[, , 1]
  } else if (is.list(result) && length(result) > 0 && is.matrix(result[[1]])) {
    mat <- result[[1]]
  } else {
    mat <- as.matrix(result)
  }
  if (!all(dim(mat) == c(length(vars), length(vars)))) {
    stop("mlVAR returned a network with unexpected dimensions.", call. = FALSE)
  }
  if (identical(type, "temporal")) {
    mat <- quicknet_from_qgraph_matrix(mat, directed = TRUE)
  }
  colnames(mat) <- rownames(mat) <- vars
  mat
}

quicknet_cluster_bootstrap_stability <- function(fit, nboot, seed, refit, originals, directed) {
  id_column <- fit$meta$id
  ids <- unique(fit$data[[id_column]])
  if (anyNA(ids) || length(ids) < 2L) {
    stop("Participant bootstrap requires at least two independent participants with non-missing IDs.", call. = FALSE)
  }
  layer_names <- names(originals)
  if (!length(layer_names)) stop("No network layers are available for bootstrap.", call. = FALSE)
  boot_arrays <- lapply(originals, function(original) array(NA_real_, c(dim(original), nboot)))
  reasons <- rep(NA_character_, nboot)
  set.seed(seed)
  for (boot_index in seq_len(nboot)) {
    sampled_ids <- ids[sample.int(length(ids), length(ids), replace = TRUE)]
    sampled_data <- do.call(rbind, lapply(seq_along(sampled_ids), function(new_id) {
      rows <- fit$data[fit$data[[id_column]] == sampled_ids[[new_id]], , drop = FALSE]
      rows[[id_column]] <- new_id
      rows
    }))
    boot_layers <- tryCatch({
      boot_fit <- refit(sampled_data, boot_index)
      failure_reason <- quicknet_fit_failure_reason(boot_fit)
      if (!is.null(failure_reason)) stop(failure_reason, call. = FALSE)
      missing_layers <- setdiff(layer_names, names(boot_fit$networks))
      if (length(missing_layers)) stop("Missing network layers: ", paste(missing_layers, collapse = ", "), call. = FALSE)
      stats::setNames(lapply(layer_names, function(layer) {
        quicknet_resampling_graph(boot_fit$networks[[layer]], originals[[layer]], layer)
      }), layer_names)
    }, error = function(e) { reasons[[boot_index]] <<- conditionMessage(e); NULL })
    if (is.null(boot_layers)) next
    for (layer in layer_names) boot_arrays[[layer]][, , boot_index] <- boot_layers[[layer]]
  }
  diagnostics <- quicknet_resampling_diagnostics(reasons, "percentile_cluster_bootstrap", "participant",
                                                 "participant bootstrap replications")
  diagnostics$participants <- length(ids)
  diagnostics$joint_layer_success <- TRUE
  diagnostics$diagonal_included <- FALSE
  out <- stats::setNames(lapply(layer_names, function(layer) {
    quicknet_matrix_bootstrap_summary(originals[[layer]], boot_arrays[[layer]],
      directed = directed[[layer]], failed_bootstraps = diagnostics$failed, diagnostics = diagnostics)
  }), layer_names)
  attr(out, "resampling") <- diagnostics
  out
}

quicknet_panel_bootstrap_stability <- function(fit, nboot, seed, nfolds) {
  refit <- function(sampled_data, boot_index) {
    ans <- quicknet_refit_with_backend_args(PanelNet, fit, sampled_data,
      nodes = fit$meta$nodes, waves = fit$meta$waves, id = fit$meta$id, prefix = fit$meta$prefix,
      standardize = fit$meta$standardize, standardize_data = fit$meta$standardize_data %||% FALSE,
      alpha = fit$meta$alpha, lambda_rule = fit$meta$lambda_rule, nfolds = nfolds, seed = seed + boot_index)
    ans$networks$default <- ans$graph
    ans
  }
  quicknet_cluster_bootstrap_stability(fit, nboot, seed, refit,
    list(default = fit$graph), c(default = TRUE))
}

quicknet_psychonetrics_panel_bootstrap_stability <- function(fit, nboot, seed) {
  layers <- setdiff(names(fit$networks), "default")
  refit <- function(sampled_data, boot_index) quicknet_refit_with_backend_args(PanelNet, fit,
    sampled_data, nodes = fit$meta$nodes, waves = fit$meta$waves, id = fit$meta$id,
    prefix = fit$meta$prefix, standardize = fit$meta$standardize, model = fit$model,
    stationary = fit$meta$stationary)
  directed <- stats::setNames(vapply(layers, function(layer) {
    quicknet_network_summary_is_directed(fit$model, fit$meta, layer)
  }, logical(1)), layers)
  quicknet_cluster_bootstrap_stability(fit, nboot, seed, refit, fit$networks[layers], directed)
}

quicknet_longitudinal_bootstrap_stability <- function(fit, nboot, seed) {
  layers <- setdiff(names(fit$networks), "default")
  refit <- function(sampled_data, boot_index) quicknet_refit_with_backend_args(LongitudinalNet, fit,
    sampled_data, vars = fit$meta$vars, id = fit$meta$id, day = fit$meta$day, beep = fit$meta$beep,
    model = fit$model, gamma = quicknet_refit_gamma(fit), scale = fit$meta$scale,
    centerWithin = fit$meta$centerWithin, lags = fit$meta$lags %||% 1,
    estimator = if (fit$model != "graphicalVAR") fit$meta$estimator else NULL,
    temporal = fit$meta$temporal, contemporaneous = fit$meta$contemporaneous,
    nCores = fit$meta$nCores %||% 1)
  directed <- stats::setNames(vapply(layers, quicknet_longitudinal_network_is_directed, logical(1)), layers)
  quicknet_cluster_bootstrap_stability(fit, nboot, seed, refit, fit$networks[layers], directed)
}

quicknet_bind_rows_fill <- function(...) {
  frames <- list(...)
  all_names <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(frame) {
    missing <- setdiff(all_names, names(frame))
    for (name in missing) {
      frame[[name]] <- NA
    }
    frame[, all_names, drop = FALSE]
  })
  do.call(rbind, frames)
}

quicknet_refit_with_backend_args <- function(fun, fit, ...) {
  args <- list(...)
  if (fit$model == "clpn" && is.null(fit$meta$standardize_data)) {
    recorded <- quicknet_fit_effective_meta(fit)
    for (name in c("standardize_data", "standardize")) {
      value <- recorded[[name]]
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        label <- if (name == "standardize_data") "data standardization" else "glmnet standardization"
        stop("The fitted object's original CLPN ", label,
             " setting is unknown; refit the original data first.", call. = FALSE)
      }
      args[[name]] <- value
    }
  }
  if (fit$model == "graphicalVAR" && is.null(fit$meta$backend_settings) &&
      is.null(fit$meta$backend_args$subjectNetworks)) args$subjectNetworks <- FALSE
  if (fit$model == "ri_clpm") args$ri_type <- fit$meta$ri_type
  if (fit$model == "mlVAR") args$centerWithin <- NULL
  do.call(fun, quicknet_merge_args(fit$meta$backend_args %||% list(), args))
}
