#' Estimate a mixed vector autoregressive network
#'
#' @param data Data frame or matrix containing time-ordered observations.
#' @param vars Variables used as nodes. Defaults to all columns.
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param lags Number of temporal lags.
#' @param lambdaSel Lambda selection method passed to \code{mgm::mvar()}.
#' @param gamma EBIC gamma passed as \code{lambdaGam}.
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
                        lags = 1,
                        lambdaSel = "EBIC",
                        gamma = 0.25,
                        scale = TRUE,
                        signInfo = TRUE,
                        ...) {
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  quicknet_dynamic_validate(dat, vars, types, levels)
  matrix_data <- as.matrix(dat[, vars, drop = FALSE])
  fit <- NULL
  invisible(utils::capture.output({
    fit <- suppressMessages(mgm::mvar(
      data = matrix_data,
      type = types,
      level = levels,
      lags = lags,
      lambdaSel = lambdaSel,
      lambdaGam = gamma,
      scale = scale,
      pbar = FALSE,
      warnings = FALSE,
      signInfo = signInfo,
      ...
    ))
  }))
  mat <- quicknet_dynamic_extract_square_matrix(fit, length(vars))
  if (is.null(mat)) {
    stop("Could not extract a temporal edge matrix from the mgm::mvar object.", call. = FALSE)
  }
  mat <- as.matrix(mat)
  colnames(mat) <- rownames(mat) <- vars
  edge_table <- quicknet_edge_table(mat, directed = TRUE, drop_zero = FALSE, include_diag = TRUE)
  edge_table$edge_type <- ifelse(edge_table$from == edge_table$to, "autoregressive", "cross_lagged")

  quicknet_fit(
    model = "mixedVAR",
    data = dat[, vars, drop = FALSE],
    networks = list(default = mat, temporal = mat),
    edges = edge_table,
    nodes = quicknet_directed_node_table(mat, network = "temporal"),
    fit = fit,
    meta = list(
      data_type = "time_series",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "mgm::mvar",
      vars = vars,
      types = types,
      levels = levels,
      lags = lags,
      lambdaSel = lambdaSel,
      gamma = gamma,
      scale = scale,
      signInfo = signInfo,
      call = match.call()
    )
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
#' @param estpoints Estimation points for local networks.
#' @param bandwidth Kernel bandwidth.
#' @param lags Number of temporal lags.
#' @param lambdaSel Lambda selection method passed to \code{mgm::tvmvar()}.
#' @param gamma EBIC gamma passed as \code{lambdaGam}.
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
                           estpoints = c(0.25, 0.50, 0.75),
                           bandwidth = 0.20,
                           lags = 1,
                           lambdaSel = "EBIC",
                           gamma = 0.25,
                           scale = TRUE,
                           ...) {
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  quicknet_dynamic_validate(dat, vars, types, levels)
  matrix_data <- as.matrix(dat[, vars, drop = FALSE])
  if (is.null(timepoints)) {
    timepoints <- seq(0, 1, length.out = nrow(matrix_data))
  }
  fit <- NULL
  invisible(utils::capture.output({
    fit <- suppressMessages(mgm::tvmvar(
      data = matrix_data,
      type = types,
      level = levels,
      timepoints = timepoints,
      estpoints = estpoints,
      bandwidth = bandwidth,
      lags = lags,
      lambdaSel = lambdaSel,
      lambdaGam = gamma,
      scale = scale,
      pbar = FALSE,
      warnings = FALSE,
      ...
    ))
  }))
  networks <- quicknet_dynamic_extract_tvmvar_networks(fit, vars, estpoints)
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
    meta = list(
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
      lambdaSel = lambdaSel,
      gamma = gamma,
      scale = scale,
      call = match.call()
    )
  )
}

quicknet_dynamic_validate <- function(dat, vars, types, levels) {
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (length(types) != length(vars) || length(levels) != length(vars)) {
    stop("types and levels must have one entry per variable.", call. = FALSE)
  }
  invisible(TRUE)
}

quicknet_dynamic_extract_square_matrix <- function(object, p) {
  if (is.matrix(object) && is.numeric(object) && all(dim(object) == c(p, p))) {
    return(object)
  }
  if (is.array(object) && is.numeric(object) && length(dim(object)) >= 3 && all(dim(object)[1:2] == c(p, p))) {
    index <- rep(1, length(dim(object)) - 2)
    return(do.call("[", c(list(object), list(seq_len(p), seq_len(p)), index, list(drop = TRUE))))
  }
  if (is.list(object)) {
    for (element in object) {
      found <- quicknet_dynamic_extract_square_matrix(element, p)
      if (!is.null(found)) return(found)
    }
  }
  NULL
}

quicknet_dynamic_extract_tvmvar_networks <- function(fit, vars, estpoints) {
  p <- length(vars)
  out <- list()
  if (!is.null(fit$wadj) && length(dim(fit$wadj)) == 4 && all(dim(fit$wadj)[1:2] == c(p, p))) {
    for (i in seq_along(estpoints)) {
      mat <- as.matrix(fit$wadj[, , 1, i])
      colnames(mat) <- rownames(mat) <- vars
      out[[paste0("estpoint_", i)]] <- mat
    }
    return(out)
  }
  if (is.list(fit)) {
    for (i in seq_along(estpoints)) {
      mat <- quicknet_dynamic_extract_square_matrix(fit[[i]], p)
      if (!is.null(mat)) {
        mat <- as.matrix(mat)
        colnames(mat) <- rownames(mat) <- vars
        out[[paste0("estpoint_", i)]] <- mat
      }
    }
  }
  out
}
