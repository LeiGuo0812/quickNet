#' Estimate a mixed vector autoregressive network
#'
#' @param data Data frame or matrix containing time-ordered observations.
#' @param vars Variables used as nodes. Defaults to all columns.
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param lags Required positive integer vector of temporal lags; the backend
#'   has no default. Use \code{lags = 1} for a first-order VAR.
#' @param lambdaSel Lambda selection method passed to \code{mgm::mvar()};
#'   its source default is CV.
#' @param gamma EBIC hyperparameter in [0,1], passed as \code{lambdaGam}.
#'   NULL selects 0.25 when lambdaSel is EBIC. Ignored for CV, with NULL
#'   recorded in metadata because gamma does not select the model.
#' @param scale Should variables be scaled by \code{mgm::mvar()}?
#' @param signInfo Should sign information be requested from \code{mgm}?
#' @param ... Additional arguments passed to \code{mgm::mvar()}.
#'
#' @return A \code{quicknet_fit} object with model \code{"mixedVAR"}.
#' @export
MixedVARNet <- function(data,
                        vars = NULL,
                        types,
                        levels,
                        lags = NULL,
                        lambdaSel = "CV",
                        gamma = NULL,
                        scale = TRUE,
                        signInfo = TRUE,
                        ...) {
  lambdaSel <- match.arg(lambdaSel, c("EBIC", "CV"))
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  if (is.null(lags)) stop("lags must be specified; the source mixed VAR estimator has no default.", call. = FALSE)
  lags <- quicknet_validate_lags(lags)
  quicknet_validate_input(
    dat,
    model = "mixedVAR",
    vars = vars,
    types = types,
    levels = levels,
    lags = lags
  )
  quicknet_dynamic_validate(dat, vars, types, levels)
  gamma <- quicknet_resolve_gamma("mixedVAR", gamma, lambdaSel)
  matrix_data <- as.matrix(dat[, vars, drop = FALSE])
  args <- quicknet_backend_args(list(...), mgm::mvar,
    reserved = c("data", "type", "level", "lags", "lambdaSel", "lambdaGam", "scale", "signInfo"))
  args <- quicknet_merge_args(list(lags = lags, lambdaSel = lambdaSel,
    scale = scale, signInfo = signInfo, pbar = FALSE), args)
  if (!is.null(gamma)) args$lambdaGam <- gamma
  fit <- do.call(mgm::mvar, c(list(data = matrix_data, type = types, level = levels), args))
  sample_info <- quicknet_dynamic_sample(fit, nrow(matrix_data), lags)
  lag_networks <- quicknet_dynamic_extract_mvar_networks(fit, vars, lags)
  if (length(lag_networks) == 0) {
    stop("Could not extract a temporal edge matrix from the mgm::mvar object.", call. = FALSE)
  }
  mat <- lag_networks[[1]]
  networks <- list(default = mat, temporal = mat)
  if (length(lag_networks) > 1) networks <- c(networks, lag_networks)
  edge_networks <- if (length(lag_networks) > 1) {
    c(list(default = mat), lag_networks)
  } else {
    list(default = mat)
  }
  edge_table <- do.call(rbind, lapply(names(edge_networks), function(network_name) {
    quicknet_edge_table(
      edge_networks[[network_name]],
      network = network_name,
      directed = TRUE,
      drop_zero = FALSE,
      include_diag = TRUE
    )
  }))
  edge_table$edge_type <- ifelse(edge_table$from == edge_table$to, "autoregressive", "cross_lagged")
  node_networks <- if (length(lag_networks) > 1) lag_networks else list(temporal = mat)
  node_table <- do.call(quicknet_bind_rows_fill, lapply(names(node_networks), function(network_name) {
    quicknet_directed_node_table(node_networks[[network_name]], network = network_name)
  }))

  quicknet_fit(
    model = "mixedVAR",
    data = dat[, vars, drop = FALSE],
    networks = networks,
    edges = edge_table,
    nodes = node_table,
    fit = fit,
    meta = c(list(
      data_type = "time_series",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "mgm::mvar",
      vars = vars,
      types = types,
      levels = levels,
      lags = lags,
      lambdaSel = fit$call$lambdaSel,
      gamma = if (isFALSE(fit$call$regularize)) NULL else quicknet_resolve_gamma("mixedVAR", fit$call$lambdaGam, fit$call$lambdaSel),
      scale = scale,
      signInfo = signInfo,
      analysis_sample = sample_info$sample,
      lag_index = sample_info$index,
      call = match.call()
    ), quicknet_backend_provenance("mgm", "mvar", args, fit$call))
  )
}

#' Estimate time-varying mixed vector autoregressive networks
#'
#' @param data Data frame or matrix containing time-ordered observations.
#' @param vars Variables used as nodes. Defaults to all columns.
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param timepoints Numeric time index scaled to the interval used by
#' \code{mgm::tvmvar()}. Defaults to an equally spaced 0-1 sequence.
#' @param estpoints Required estimation points for local networks; no source default.
#' @param bandwidth Required kernel bandwidth; no source default.
#' @param lags Required positive integer vector of temporal lags; the backend
#'   has no default. Use \code{lags = 1} for a first-order VAR.
#' @param lambdaSel Lambda selection method passed to \code{mgm::tvmvar()};
#'   its source default is EBIC.
#' @param gamma EBIC hyperparameter in [0,1], passed as \code{lambdaGam}.
#'   NULL selects 0.25 when lambdaSel is EBIC. Ignored for CV, with NULL
#'   recorded in metadata because gamma does not select the model.
#' @param scale Should variables be scaled by \code{mgm::tvmvar()}?
#' @param ... Additional arguments passed to \code{mgm::tvmvar()}.
#'
#' @return A \code{quicknet_fit} object with model \code{"time_varying_mvar"}.
#' @export
TimeVaryingNet <- function(data,
                           vars = NULL,
                           types,
                           levels,
                           timepoints = NULL,
                           estpoints = NULL,
                           bandwidth = NULL,
                           lags = NULL,
                           lambdaSel = "EBIC",
                           gamma = NULL,
                           scale = TRUE,
                           ...) {
  if (is.null(estpoints) || is.null(bandwidth)) {
    stop("estpoints and bandwidth must be specified; mgm::tvmvar has no defaults for them.", call. = FALSE)
  }
  lambdaSel <- match.arg(lambdaSel, c("EBIC", "CV"))
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  if (is.null(lags)) stop("lags must be specified; the source mixed VAR estimator has no default.", call. = FALSE)
  lags <- quicknet_validate_lags(lags)
  quicknet_validate_input(
    dat,
    model = "time_varying_mvar",
    vars = vars,
    types = types,
    levels = levels,
    lags = lags,
    timepoints = timepoints,
    estpoints = estpoints,
    bandwidth = bandwidth
  )
  quicknet_dynamic_validate(dat, vars, types, levels)
  gamma <- quicknet_resolve_gamma("time_varying_mvar", gamma, lambdaSel)
  matrix_data <- as.matrix(dat[, vars, drop = FALSE])
  if (is.null(timepoints)) {
    timepoints <- seq(0, 1, length.out = nrow(matrix_data))
  }
  if (!is.numeric(timepoints) || length(timepoints) != nrow(matrix_data) ||
      any(!is.finite(timepoints)) || is.unsorted(timepoints, strictly = TRUE)) {
    stop("timepoints must be a strictly increasing finite numeric vector with one value per row.", call. = FALSE)
  }
  args <- quicknet_backend_args(list(...), mgm::tvmvar,
    reserved = c("data", "type", "level", "timepoints", "estpoints", "bandwidth", "lags", "lambdaSel", "lambdaGam", "scale"),
    extra = setdiff(names(formals(mgm::mvar)), "..."))
  args <- quicknet_merge_args(list(lags = lags, lambdaSel = lambdaSel, scale = scale, pbar = FALSE), args)
  if (!is.null(gamma)) args$lambdaGam <- gamma
  fit <- do.call(mgm::tvmvar, c(list(data = matrix_data, type = types, level = levels,
    timepoints = timepoints, estpoints = estpoints, bandwidth = bandwidth), args))
  sample_info <- lapply(fit$tvmodels, quicknet_dynamic_sample, n = nrow(matrix_data), lags = lags)
  local_samples <- if (length(sample_info)) do.call(rbind, lapply(seq_along(sample_info), function(i) {
    x <- sample_info[[i]]$sample
    data.frame(estpoint = estpoints[[i]], temporal_rows = x$temporal_rows,
      positive_weight_rows = x$positive_weight_rows, weight_sum = x$weight_sum)
  })) else data.frame()
  networks <- quicknet_dynamic_extract_tvmvar_networks(fit, vars, estpoints, lags)
  if (length(networks) == 0) {
    stop("Could not extract local networks from the mgm::tvmvar object.", call. = FALSE)
  }
  networks <- c(list(default = networks[[1]]), networks)
  edges <- do.call(rbind, lapply(names(networks), function(network_name) {
    quicknet_edge_table(networks[[network_name]], network = network_name, directed = TRUE, drop_zero = FALSE, include_diag = TRUE)
  }))
  edges$edge_type <- ifelse(edges$from == edges$to, "autoregressive", "cross_lagged")
  nodes <- do.call(quicknet_bind_rows_fill, lapply(names(networks), function(network_name) {
    quicknet_directed_node_table(networks[[network_name]], network = network_name)
  }))

  quicknet_fit(
    model = "time_varying_mvar",
    data = dat[, vars, drop = FALSE],
    networks = networks,
    edges = edges,
    nodes = nodes,
    fit = fit,
    meta = c(list(
      data_type = "time_series",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "mgm::tvmvar",
      vars = vars,
      types = types,
      levels = levels,
      timepoints = timepoints,
      estpoints = estpoints,
      bandwidth = bandwidth,
      lags = lags,
      lambdaSel = fit$call$lambdaSel,
      gamma = if (isFALSE(fit$call$regularize)) NULL else quicknet_resolve_gamma("time_varying_mvar", fit$call$lambdaGam, fit$call$lambdaSel),
      scale = scale,
      analysis_sample = list(input_rows = nrow(matrix_data), complete_rows = nrow(matrix_data),
        temporal_rows = if (nrow(local_samples)) local_samples$temporal_rows[[1L]] else NA_integer_,
        local_samples = local_samples,
        counts_source = "mgm local models: data_lagged$included and weights_design"),
      lag_index = if (length(sample_info)) sample_info[[1L]]$index else NULL,
      call = match.call()
    ), quicknet_backend_provenance("mgm", "tvmvar", args, fit$call))
  )
}

quicknet_dynamic_sample <- function(fit, n, lags) {
  included <- fit$call$data_lagged$included
  weights <- fit$call$weights_design
  known <- is.logical(included) && length(included) == n && !anyNA(included)
  index <- if (known) do.call(rbind, lapply(lags, function(lag) {
    rows <- seq_len(n)
    previous <- rows - lag
    previous[previous < 1L] <- NA_integer_
    data.frame(row = rows, lag = lag, predecessor = previous, included = included)
  })) else NULL
  list(sample = list(input_rows = n, complete_rows = n,
    temporal_rows = if (known) sum(included) else NA_integer_,
    positive_weight_rows = if (!is.null(weights)) sum(weights > 0) else NA_integer_,
    weight_sum = if (!is.null(weights)) sum(weights) else NA_real_,
    counts_source = "mgm::mvar call$data_lagged$included and weights_design"), index = index)
}

quicknet_dynamic_validate <- function(dat, vars, types, levels) {
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (length(types) != length(vars) || length(levels) != length(vars)) {
    stop("types and levels must have one entry per variable.", call. = FALSE)
  }
  if (!is.character(types) || anyNA(types) || any(!types %in% c("g", "p", "c"))) {
    stop("types must contain only 'g', 'p', or 'c'.", call. = FALSE)
  }
  if (!is.numeric(levels) || any(!is.finite(levels)) ||
      any(levels < 1 | levels != floor(levels))) {
    stop("levels must contain positive integers.", call. = FALSE)
  }

  selected <- dat[, vars, drop = FALSE]
  numeric_columns <- vapply(selected, is.numeric, logical(1))
  if (!all(numeric_columns)) {
    stop(
      "All dynamic MGM variables must be numeric or integer.",
      call. = FALSE
    )
  }
  if (anyNA(selected) || any(!is.finite(as.matrix(selected)))) {
    stop("Dynamic MGM data must not contain missing or non-finite values.", call. = FALSE)
  }
  noncategorical <- types != "c"
  if (any(levels[noncategorical] != 1)) {
    stop("Gaussian and Poisson variables must have level 1.", call. = FALSE)
  }
  categorical <- which(types == "c")
  if (length(categorical) > 0) {
    observed_levels <- vapply(selected[categorical], function(x) length(unique(x)), integer(1))
    if (any(levels[categorical] != observed_levels)) {
      stop(
        "Each categorical level entry must match the number of observed categories.",
        call. = FALSE
      )
    }
  }
  poisson <- which(types == "p")
  if (length(poisson) > 0) {
    valid_poisson <- vapply(
      selected[poisson],
      function(x) all(x >= 0 & x == floor(x)),
      logical(1)
    )
    if (!all(valid_poisson)) {
      stop("Poisson variables must contain non-negative integers.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

quicknet_dynamic_extract_mvar_networks <- function(fit, vars, lags) {
  p <- length(vars)
  lag_values <- as.integer(lags)
  weights <- fit$wadj
  signs <- fit$signs
  if (is.matrix(weights) && all(dim(weights) == c(p, p)) && length(lag_values) == 1) {
    weights <- array(weights, dim = c(p, p, 1))
  }
  if (is.null(weights) || length(dim(weights)) != 3 ||
      !all(dim(weights)[1:2] == c(p, p)) ||
      dim(weights)[3] < length(lag_values)) {
    return(list())
  }

  out <- stats::setNames(vector("list", length(lag_values)), paste0("lag_", lag_values))
  for (lag_index in seq_along(lag_values)) {
    lag_signs <- if (!is.null(signs) && length(dim(signs)) == 3 &&
      all(dim(signs)[1:2] == c(p, p)) && dim(signs)[3] >= lag_index) {
      signs[, , lag_index]
    } else {
      NULL
    }
    mat <- quicknet_apply_signs(weights[, , lag_index], lag_signs)
    colnames(mat) <- rownames(mat) <- vars
    out[[lag_index]] <- mat
  }
  out
}

quicknet_dynamic_extract_tvmvar_networks <- function(fit, vars, estpoints, lags = 1) {
  p <- length(vars)
  out <- list()
  lag_values <- as.integer(lags)
  if (!is.null(fit$wadj) && length(dim(fit$wadj)) == 4 &&
      all(dim(fit$wadj)[1:2] == c(p, p)) &&
      dim(fit$wadj)[3] >= length(lag_values) &&
      dim(fit$wadj)[4] >= length(estpoints)) {
    for (i in seq_along(estpoints)) {
      for (lag_index in seq_along(lag_values)) {
        lag_signs <- if (!is.null(fit$signs) && length(dim(fit$signs)) == 4 &&
          all(dim(fit$signs)[1:2] == c(p, p)) &&
          dim(fit$signs)[3] >= lag_index &&
          dim(fit$signs)[4] >= i) {
          fit$signs[, , lag_index, i]
        } else {
          NULL
        }
        mat <- quicknet_apply_signs(fit$wadj[, , lag_index, i], lag_signs)
        colnames(mat) <- rownames(mat) <- vars
        if (lag_index == 1) {
          out[[paste0("estpoint_", i)]] <- mat
        }
        if (length(lag_values) > 1) {
          out[[paste0("estpoint_", i, "_lag_", lag_values[[lag_index]])]] <- mat
        }
      }
    }
    return(out)
  }
  if (is.list(fit$tvmodels) && length(fit$tvmodels) >= length(estpoints)) {
    for (i in seq_along(estpoints)) {
      local_networks <- quicknet_dynamic_extract_mvar_networks(fit$tvmodels[[i]], vars, lags)
      if (length(local_networks) > 0) {
        out[[paste0("estpoint_", i)]] <- local_networks[[1]]
        if (length(local_networks) > 1) {
          for (lag_index in seq_along(local_networks)) {
            out[[paste0("estpoint_", i, "_", names(local_networks)[[lag_index]])]] <-
              local_networks[[lag_index]]
          }
        }
      }
    }
  }
  out
}
