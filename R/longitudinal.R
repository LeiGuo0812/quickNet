#' Estimate a cross-lagged panel network
#'
#' @param data Wide-format panel data.
#' @param nodes Base node names without wave suffix.
#' @param waves Wave identifiers used in column names.
#' @param id ID variable. If absent, row numbers are used as IDs.
#' @param prefix Separator between node and wave in wide columns.
#' @param standardize Should panel variables be standardized before fitting?
#' @param alpha Elastic-net mixing parameter passed to \code{glmnet}.
#' @param lambda_rule Lambda selection rule. One of \code{"lambda.1se"} or \code{"lambda.min"}.
#' @param nfolds Number of cross-validation folds.
#' @param seed Random seed used for cross-validation folds.
#' @param model Panel model. One of \code{"clpn"}, \code{"ri_clpm"},
#' \code{"panel_gvar"}, or \code{"panel_var"}.
#' @param ri_type Innovation structure used by \code{psychonetrics::ri_clpm()}.
#' @param stationary Optional stationarity constraints passed to
#' \code{psychonetrics::ri_clpm_stationary()}.
#' @param ... Additional arguments passed to psychonetrics panel backends.
#'
#' @return A \code{quicknet_fit} object. The edge matrix uses rows as outcomes/to nodes and columns as predictors/from nodes.
#' @export
PanelNet <- function(data,
                     nodes,
                     waves,
                     id = "id",
                     prefix = "_t",
                     standardize = TRUE,
                     alpha = 1,
                     lambda_rule = c("lambda.1se", "lambda.min"),
                     nfolds = 10,
                     seed = 20260502,
                     model = "clpn",
                     ri_type = c("ggm", "cov", "chol", "prec"),
                     stationary = NULL,
                     ...) {
  model <- match.arg(model, c("clpn", "ri_clpm", "panel_gvar", "panel_var"))
  lambda_rule <- match.arg(lambda_rule)
  ri_type <- match.arg(ri_type)
  quicknet_validate_input(
    data,
    model = model,
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix
  )
  if (model != "clpn") {
    return(quicknet_psychonetrics_panel_fit(
      data = data,
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
    panel_data = data,
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix,
    standardize = standardize
  )
  fit <- quicknet_clpn_glmnet(
    predictors = design$predictors,
    outcomes = design$outcomes,
    alpha = alpha,
    lambda_rule = lambda_rule,
    nfolds = nfolds,
    seed = seed
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
    data = data,
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
      alpha = alpha,
      lambda_rule = lambda_rule,
      nfolds = nfolds,
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
#' @param day Day variable.
#' @param beep Beep or measurement-occasion variable within day.
#' @param model Longitudinal model. One of \code{"graphicalVAR"},
#' \code{"mlVAR"}, or \code{"psychonetrics_gvar"}.
#' @param gamma EBIC gamma used by \code{graphicalVAR::mlGraphicalVAR()}.
#' @param scale Should variables be scaled?
#' @param centerWithin Should variables be person-mean centered?
#' @param lags Number of lags used by \code{mlVAR}.
#' @param estimator Estimator used by \code{mlVAR}.
#' @param temporal Temporal effect structure used by \code{mlVAR}.
#' @param contemporaneous Contemporaneous effect structure used by \code{mlVAR}.
#' @param nCores Number of cores used by \code{mlVAR}.
#' @param ... Additional arguments passed to the selected backend.
#'
#' @return A \code{quicknet_fit} object with \code{temporal}, \code{contemporaneous}, and \code{between} networks.
#' @export
LongitudinalNet <- function(data,
                            vars,
                            id = "id",
                            day = "day",
                            beep = "beep",
                            model = "graphicalVAR",
                            gamma = 0.5,
                            scale = TRUE,
                            centerWithin = TRUE,
                            lags = 1,
                            estimator = "lmer",
                            temporal = "fixed",
                            contemporaneous = "fixed",
                            nCores = 1,
                            ...) {
  model <- match.arg(model, c("graphicalVAR", "mlVAR", "psychonetrics_gvar"))
  temporal_setting <- temporal
  contemporaneous_setting <- contemporaneous
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
  dat <- data[order(data[[id]], data[[day]], data[[beep]]), , drop = FALSE]

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
      call = match.call(),
      ...
    ))
  }

  if (model == "graphicalVAR") {
    if (!requireNamespace("graphicalVAR", quietly = TRUE)) {
      stop("Package 'graphicalVAR' is required for LongitudinalNet(model = 'graphicalVAR').", call. = FALSE)
    }
    invisible(utils::capture.output({
      fit <- suppressMessages(graphicalVAR::mlGraphicalVAR(
        data = dat,
        vars = vars,
        beepvar = beep,
        dayvar = day,
        idvar = id,
        scale = scale,
        centerWithin = centerWithin,
        gamma = gamma,
        verbose = FALSE,
        subjectNetworks = FALSE,
        ...
      ))
    }))
    temporal <- as.matrix(fit$fixedPDC)
    contemporaneous <- as.matrix(fit$fixedPCC)
    between <- as.matrix(fit$betweenNet)
  } else {
    if (!requireNamespace("mlVAR", quietly = TRUE)) {
      stop("Package 'mlVAR' is required for LongitudinalNet(model = 'mlVAR').", call. = FALSE)
    }
    invisible(utils::capture.output({
      fit <- suppressMessages(mlVAR::mlVAR(
        data = dat,
        vars = vars,
        idvar = id,
        lags = lags,
        dayvar = day,
        beepvar = beep,
        estimator = estimator,
        temporal = temporal_setting,
        contemporaneous = contemporaneous_setting,
        nCores = nCores,
        verbose = FALSE,
        scale = scale,
        ...
      ))
    }))
    temporal <- quicknet_mlvar_get_net(fit, "temporal", vars)
    contemporaneous <- quicknet_mlvar_get_net(fit, "contemporaneous", vars)
    between <- quicknet_mlvar_get_net(fit, "between", vars)
  }
  colnames(temporal) <- rownames(temporal) <- vars
  colnames(contemporaneous) <- rownames(contemporaneous) <- vars
  colnames(between) <- rownames(between) <- vars

  edges <- rbind(
    quicknet_edge_table(temporal, network = "temporal", directed = TRUE, drop_zero = FALSE, include_diag = TRUE),
    quicknet_edge_table(contemporaneous, network = "contemporaneous", directed = FALSE, drop_zero = FALSE),
    quicknet_edge_table(between, network = "between", directed = FALSE, drop_zero = FALSE)
  )
  nodes <- quicknet_bind_rows_fill(
    quicknet_directed_node_table(temporal, network = "temporal"),
    quicknet_node_table(contemporaneous, network = "contemporaneous"),
    quicknet_node_table(between, network = "between")
  )

  quicknet_fit(
    model = model,
    data = dat,
    networks = list(default = temporal, temporal = temporal, contemporaneous = contemporaneous, between = between),
    edges = edges,
    nodes = nodes,
    fit = fit,
    meta = list(
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
      centerWithin = centerWithin,
      lags = if (model == "mlVAR") lags else NULL,
      estimator = if (model == "mlVAR") estimator else NULL,
      temporal = if (model == "mlVAR") temporal_setting else NULL,
      contemporaneous = if (model == "mlVAR") contemporaneous_setting else NULL,
      nCores = if (model == "mlVAR") nCores else NULL,
      call = match.call()
    )
  )
}

#' Bootstrap stability for longitudinal network fits
#'
#' @param fit A \code{quicknet_fit} object returned by \code{PanelNet()} or \code{LongitudinalNet()}.
#' @param nboot Number of bootstrap samples.
#' @param seed Random seed.
#' @param nfolds Number of CV folds used when refitting CLPN.
#'
#' @return A named list of bootstrap stability tables.
#' @export
LongitudinalStability <- function(fit, nboot = 100, seed = 20260502, nfolds = NULL) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  if (!fit$model %in% c("clpn", "graphicalVAR", "mlVAR", "psychonetrics_gvar")) {
    stop("LongitudinalStability() supports CLPN, graphicalVAR, mlVAR, and psychonetrics_gvar fits.", call. = FALSE)
  }
  set.seed(seed)
  if (fit$model == "clpn") {
    return(quicknet_panel_bootstrap_stability(fit, nboot = nboot, seed = seed, nfolds = nfolds %||% fit$meta$nfolds))
  }
  quicknet_longitudinal_bootstrap_stability(fit, nboot = nboot, seed = seed)
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
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
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
    nodes = nodes,
    waves = waves
  )
}

quicknet_clpn_glmnet <- function(predictors,
                                 outcomes,
                                 alpha = 1,
                                 lambda_rule = c("lambda.1se", "lambda.min"),
                                 nfolds = 10,
                                 seed = 20260502) {
  lambda_rule <- match.arg(lambda_rule)
  predictors <- as.matrix(predictors)
  outcomes <- as.matrix(outcomes)
  nodes <- colnames(outcomes)
  edge_matrix <- matrix(0, nrow = ncol(outcomes), ncol = ncol(predictors), dimnames = list(nodes, colnames(predictors)))
  predictability <- data.frame(node = nodes, cv_r_squared = NA_real_, lambda = NA_real_, nonzero_predictors = NA_integer_)
  fits <- list()

  nfolds <- max(3, min(nfolds, nrow(predictors)))
  set.seed(seed)
  foldid <- sample(rep(seq_len(nfolds), length.out = nrow(predictors)))

  for (target in nodes) {
    y <- outcomes[, target]
    cv_fit <- glmnet::cv.glmnet(
      x = predictors,
      y = y,
      alpha = alpha,
      family = "gaussian",
      standardize = FALSE,
      nfolds = nfolds,
      foldid = foldid
    )
    lambda_value <- if (lambda_rule == "lambda.1se") cv_fit$lambda.1se else cv_fit$lambda.min
    coefficients <- as.matrix(stats::coef(cv_fit, s = lambda_value))
    edge_matrix[target, ] <- as.numeric(coefficients[colnames(predictors), 1])
    cv_mse <- cv_fit$cvm[which.min(abs(cv_fit$lambda - lambda_value))]
    null_mse <- stats::var(y)
    predictability[predictability$node == target, c("cv_r_squared", "lambda", "nonzero_predictors")] <- c(
      ifelse(is.finite(null_mse) && null_mse > 0, 1 - cv_mse / null_mse, NA_real_),
      lambda_value,
      sum(abs(edge_matrix[target, ]) > 1e-10)
    )
    fits[[target]] <- cv_fit
  }

  list(edge_matrix = edge_matrix, predictability = predictability, fits = fits, lambda_rule = lambda_rule)
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
    standardize = standardize
  )
  vars_matrix <- quicknet_panel_vars_matrix(nodes, waves, prefix)

  raw_model <- switch(
    model,
    ri_clpm = psychonetrics::ri_clpm(dat$data, vars = vars_matrix, type = ri_type, verbose = FALSE, ...),
    panel_gvar = psychonetrics::panelgvar(dat$data, vars = vars_matrix, verbose = FALSE, ...),
    panel_var = psychonetrics::panelvar(dat$data, vars = vars_matrix, verbose = FALSE, ...)
  )
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
    if (network_name %in% c("default", "temporal")) {
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
      call = call
    )
  )
}

quicknet_psychonetrics_gvar_fit <- function(data,
                                            vars,
                                            id,
                                            day,
                                            beep,
                                            scale,
                                            call,
                                            ...) {
  standardize <- if (isTRUE(scale)) "z" else "none"
  raw_model <- psychonetrics::gvar(
    data = data,
    vars = vars,
    idvar = id,
    dayvar = day,
    beepvar = beep,
    standardize = standardize,
    verbose = FALSE,
    ...
  )
  fit <- quicknet_psychonetrics_run(raw_model)
  temporal <- quicknet_psychonetrics_matrix(fit, "beta", vars)
  contemporaneous <- quicknet_psychonetrics_first_matrix(fit, c("omega_zeta", "sigma_zeta", "kappa_zeta"), vars)
  networks <- list(default = temporal, temporal = temporal, contemporaneous = contemporaneous)
  edges <- quicknet_longitudinal_edges(networks)
  nodes <- quicknet_bind_rows_fill(
    quicknet_directed_node_table(temporal, network = "temporal"),
    quicknet_node_table(contemporaneous, network = "contemporaneous")
  )

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
      call = call
    )
  )
}

quicknet_panel_psychonetrics_data <- function(data, nodes, waves, id, prefix, standardize) {
  dat <- as.data.frame(data)
  required_columns <- unlist(lapply(waves, function(wave) paste0(nodes, prefix, wave)))
  if (!id %in% colnames(dat)) dat[[id]] <- seq_len(nrow(dat))
  dat <- dat[, c(id, required_columns), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
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
  invisible(utils::capture.output({
    out <- suppressMessages(psychonetrics::runmodel(model))
  }))
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
  mat <- tryCatch(psychonetrics::getmatrix(fit, matrix_name), error = function(e) NULL)
  if (is.null(mat)) {
    mat <- base::matrix(NA_real_, length(vars), length(vars))
  }
  mat <- as.matrix(mat)
  if (!all(dim(mat) == c(length(vars), length(vars)))) {
    mat <- mat[seq_len(length(vars)), seq_len(length(vars)), drop = FALSE]
  }
  colnames(mat) <- rownames(mat) <- vars
  mat
}

quicknet_psychonetrics_first_matrix <- function(fit, candidates, vars) {
  available <- tryCatch(fit@matrices$name, error = function(e) character())
  for (candidate in candidates) {
    if (candidate %in% available) {
      mat <- quicknet_psychonetrics_matrix(fit, candidate, vars)
      diag(mat) <- 0
      return(mat)
    }
  }
  base::matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
}

quicknet_ri_clpm_networks <- function(fit, nodes, waves, prefix) {
  pars <- data.frame()
  invisible(utils::capture.output({
    pars <- tryCatch(psychonetrics::parameters(fit), error = function(e) data.frame())
  }))
  temporal <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  contemporaneous <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  random_intercept <- matrix(NA_real_, length(nodes), length(nodes), dimnames = list(nodes, nodes))

  for (from_node in nodes) {
    for (to_node in nodes) {
      values <- numeric()
      for (wave_index in seq_len(length(waves) - 1)) {
        from_name <- paste0("C_", from_node, prefix, waves[[wave_index]])
        to_name <- paste0("C_", to_node, prefix, waves[[wave_index + 1]])
        row <- pars[pars$matrix == "beta" & pars$var1 == to_name & pars$var2 == from_name, , drop = FALSE]
        if (nrow(row) > 0) values <- c(values, row$est)
      }
      temporal[to_node, from_node] <- if (length(values) > 0) mean(values, na.rm = TRUE) else NA_real_
    }
  }

  for (node_i in nodes) {
    for (node_j in nodes) {
      if (node_i == node_j) {
        contemporaneous[node_i, node_j] <- 0
        random_intercept[node_i, node_j] <- 0
        next
      }
      innovation_values <- numeric()
      for (wave in waves) {
        name_i <- paste0("C_", node_i, prefix, wave)
        name_j <- paste0("C_", node_j, prefix, wave)
        row <- pars[
          pars$matrix == "omega_zeta" &
            ((pars$var1 == name_i & pars$var2 == name_j) | (pars$var1 == name_j & pars$var2 == name_i)),
          ,
          drop = FALSE
        ]
        if (nrow(row) > 0) innovation_values <- c(innovation_values, row$est)
      }
      contemporaneous[node_i, node_j] <- if (length(innovation_values) > 0) mean(innovation_values, na.rm = TRUE) else NA_real_
      ri_i <- paste0("RI_", node_i)
      ri_j <- paste0("RI_", node_j)
      ri_row <- pars[
        pars$matrix == "omega_zeta" &
          ((pars$var1 == ri_i & pars$var2 == ri_j) | (pars$var1 == ri_j & pars$var2 == ri_i)),
        ,
        drop = FALSE
      ]
      random_intercept[node_i, node_j] <- if (nrow(ri_row) > 0) mean(ri_row$est, na.rm = TRUE) else NA_real_
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
    directed <- network_name %in% c("default", "temporal", "cross_lagged")
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

quicknet_mlvar_get_net <- function(fit, type, vars) {
  result <- tryCatch(mlVAR::getNet(fit, type = type, nonsig = "show"), error = function(e) NULL)
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
  colnames(mat) <- rownames(mat) <- vars
  mat
}

quicknet_panel_bootstrap_stability <- function(fit, nboot, seed, nfolds) {
  ids <- unique(fit$data[[fit$meta$id]])
  nodes <- fit$meta$nodes
  edge_array <- array(
    NA_real_,
    dim = c(length(nodes), length(nodes), nboot),
    dimnames = list(nodes, nodes, paste0("boot_", seq_len(nboot)))
  )
  failed <- logical(nboot)
  set.seed(seed)
  for (boot_index in seq_len(nboot)) {
    sampled_ids <- sample(ids, length(ids), replace = TRUE)
    sampled_data <- do.call(rbind, lapply(seq_along(sampled_ids), function(new_id) {
      rows <- fit$data[fit$data[[fit$meta$id]] == sampled_ids[new_id], , drop = FALSE]
      rows[[fit$meta$id]] <- new_id
      rows
    }))
    boot_fit <- tryCatch(
      PanelNet(
        sampled_data,
        nodes = fit$meta$nodes,
        waves = fit$meta$waves,
        id = fit$meta$id,
        prefix = fit$meta$prefix,
        standardize = fit$meta$standardize,
        alpha = fit$meta$alpha,
        lambda_rule = fit$meta$lambda_rule,
        nfolds = nfolds,
        seed = seed + boot_index
      ),
      error = function(e) NULL
    )
    if (is.null(boot_fit)) {
      failed[boot_index] <- TRUE
      next
    }
    edge_array[, , boot_index] <- boot_fit$graph
  }

  list(
    default = quicknet_matrix_bootstrap_summary(
      original_matrix = fit$graph,
      edge_array = edge_array,
      directed = TRUE,
      failed_bootstraps = sum(failed)
    )
  )
}

quicknet_longitudinal_bootstrap_stability <- function(fit, nboot, seed) {
  ids <- unique(fit$data[[fit$meta$id]])
  vars <- fit$meta$vars
  template <- array(
    NA_real_,
    dim = c(length(vars), length(vars), nboot),
    dimnames = list(vars, vars, paste0("boot_", seq_len(nboot)))
  )
  boot_arrays <- list(temporal = template, contemporaneous = template, between = template)
  failed <- logical(nboot)
  set.seed(seed)

  for (boot_index in seq_len(nboot)) {
    sampled_ids <- sample(ids, length(ids), replace = TRUE)
    sampled_data <- do.call(rbind, lapply(seq_along(sampled_ids), function(new_id) {
      rows <- fit$data[fit$data[[fit$meta$id]] == sampled_ids[new_id], , drop = FALSE]
      rows[[fit$meta$id]] <- new_id
      rows
    }))
    boot_fit <- tryCatch(
      LongitudinalNet(
        sampled_data,
        vars = fit$meta$vars,
        id = fit$meta$id,
        day = fit$meta$day,
        beep = fit$meta$beep,
        model = fit$model,
        gamma = fit$meta$gamma,
        scale = fit$meta$scale,
        centerWithin = fit$meta$centerWithin,
        lags = fit$meta$lags %||% 1,
        estimator = fit$meta$estimator %||% "lmer",
        temporal = fit$meta$temporal %||% "fixed",
        contemporaneous = fit$meta$contemporaneous %||% "fixed",
        nCores = fit$meta$nCores %||% 1
      ),
      error = function(e) NULL
    )
    if (is.null(boot_fit)) {
      failed[boot_index] <- TRUE
      next
    }
    boot_arrays$temporal[, , boot_index] <- boot_fit$networks$temporal
    boot_arrays$contemporaneous[, , boot_index] <- boot_fit$networks$contemporaneous
    boot_arrays$between[, , boot_index] <- boot_fit$networks$between
  }

  list(
    temporal = quicknet_matrix_bootstrap_summary(fit$networks$temporal, boot_arrays$temporal, directed = TRUE, failed_bootstraps = sum(failed)),
    contemporaneous = quicknet_matrix_bootstrap_summary(fit$networks$contemporaneous, boot_arrays$contemporaneous, directed = FALSE, failed_bootstraps = sum(failed)),
    between = quicknet_matrix_bootstrap_summary(fit$networks$between, boot_arrays$between, directed = FALSE, failed_bootstraps = sum(failed))
  )
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
