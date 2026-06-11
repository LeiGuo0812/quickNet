#' Estimate a confirmatory Gaussian graphical model
#'
#' @param data Data frame containing network variables.
#' @param vars Variables used as nodes. Defaults to all columns.
#' @param omega Optional symmetric template matrix. Nonzero entries are freely
#' estimated and zero entries are fixed to zero. If \code{NULL}, all off-diagonal
#' edges are freely estimated.
#' @param estimator Estimator passed to \code{psychonetrics::ggm()}.
#' @param missing Missing-data handling passed to \code{psychonetrics::ggm()}.
#'
#' @return A \code{quicknet_fit} object with model \code{"confirmatory_ggm"}.
#' @export
ConfirmatoryNet <- function(data,
                            vars = NULL,
                            omega = NULL,
                            estimator = "ML",
                            missing = "listwise") {
  if (!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("Package 'psychonetrics' is required for ConfirmatoryNet().", call. = FALSE)
  }
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  dat <- dat[, vars, drop = FALSE]
  dat <- quicknet_complete_numeric_data(dat, missing = "listwise")
  node_names <- colnames(dat)
  omega_template <- quicknet_confirmatory_template(omega, node_names)

  model <- psychonetrics::ggm(
    data = dat,
    vars = node_names,
    omega = omega_template,
    estimator = estimator,
    missing = missing
  )
  model <- psychonetrics::runmodel(model)
  mat <- as.matrix(psychonetrics::getmatrix(model, "omega"))
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- node_names
  fit_object <- NULL
  invisible(utils::capture.output({
    fit_object <- psychonetrics::fit(model)
  }))
  fit_indices <- as.data.frame(fit_object)

  quicknet_fit(
    model = "confirmatory_ggm",
    data = dat,
    networks = list(default = mat),
    nodes = quicknet_node_table(mat),
    fit = list(model = model, fit_indices = fit_indices, omega_template = omega_template),
    meta = list(
      data_type = "cross_sectional",
      directed = FALSE,
      backend = "psychonetrics::ggm",
      estimator = estimator,
      missing = missing,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    )
  )
}

#' Estimate latent and residual networks from a CFA model
#'
#' @param data Data frame containing manifest variables.
#' @param model Lavaan CFA model syntax.
#' @param vars Manifest variables used in the CFA. Defaults to variables found
#' in \code{data}.
#' @param std.lv Should latent variables be standardized in \code{lavaan::cfa()}?
#' @param missing Missing-data handling passed to \code{lavaan::cfa()}.
#' @param residual Should a residual item network be returned?
#'
#' @return A \code{quicknet_fit} object with model \code{"latent_network"}.
#' @export
LatentNet <- function(data,
                      model,
                      vars = NULL,
                      std.lv = TRUE,
                      missing = "listwise",
                      residual = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required for LatentNet().", call. = FALSE)
  }
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  dat <- dat[, vars, drop = FALSE]
  dat <- quicknet_complete_numeric_data(dat, missing = "listwise")

  fit <- lavaan::cfa(model = model, data = dat, std.lv = std.lv, missing = missing)
  latent <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
  diag(latent) <- 0
  latent_names <- colnames(latent)
  if (is.null(latent_names)) latent_names <- paste0("F", seq_len(ncol(latent)))
  colnames(latent) <- rownames(latent) <- latent_names

  networks <- list(default = latent, latent = latent)
  node_tables <- list(quicknet_node_table(latent, network = "latent"))
  residual_matrix <- NULL
  if (isTRUE(residual)) {
    residual_matrix <- quicknet_lavaan_residual_network(fit, dat)
    networks$residual <- residual_matrix
    node_tables[[length(node_tables) + 1]] <- quicknet_node_table(residual_matrix, network = "residual")
  }
  fit_indices <- quicknet_lavaan_fit_indices(fit)
  loadings <- quicknet_lavaan_loadings(fit)

  quicknet_fit(
    model = "latent_network",
    data = dat,
    networks = networks,
    nodes = do.call(quicknet_bind_rows_fill, node_tables),
    fit = list(model = fit, fit_indices = fit_indices, loadings = loadings, residual = residual_matrix),
    meta = list(
      data_type = "latent",
      directed = FALSE,
      backend = "lavaan::cfa",
      std.lv = std.lv,
      missing = missing,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    )
  )
}

#' Estimate a SEM-based cross-lagged panel network
#'
#' @param data Wide-format panel data.
#' @param nodes Base node names without wave suffix.
#' @param waves Wave identifiers used in column names.
#' @param id Optional ID variable.
#' @param prefix Separator between node and wave in wide columns.
#' @param standardize Should panel variables be standardized before fitting?
#' @param residual_cov Should same-wave residual covariances be freely estimated
#' for waves after the first wave?
#' @param missing Missing-data handling passed to \code{lavaan::sem()}.
#'
#' @return A \code{quicknet_fit} object with model \code{"panel_sem"}.
#' @export
PanelSEMNet <- function(data,
                        nodes,
                        waves,
                        id = "id",
                        prefix = "_t",
                        standardize = TRUE,
                        residual_cov = TRUE,
                        missing = "listwise") {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required for PanelSEMNet().", call. = FALSE)
  }
  if (length(waves) < 2) {
    stop("At least two waves are required for PanelSEMNet().", call. = FALSE)
  }
  dat <- as.data.frame(data)
  required_columns <- unlist(lapply(waves, function(wave) paste0(nodes, prefix, wave)))
  missing_cols <- setdiff(required_columns, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing panel columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!id %in% colnames(dat)) dat[[id]] <- seq_len(nrow(dat))
  dat <- dat[, c(id, required_columns), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (standardize) {
    dat[required_columns] <- lapply(dat[required_columns], function(x) as.numeric(scale(x)))
  }

  syntax <- quicknet_panel_sem_syntax(nodes, waves, prefix, residual_cov = residual_cov)
  fit <- lavaan::sem(syntax, data = dat, fixed.x = FALSE, missing = missing)
  parameters <- lavaan::standardizedSolution(fit)
  path_table <- parameters[parameters$op == "~", c("lhs", "rhs", "est.std", "se", "z", "pvalue")]
  mat <- quicknet_panel_sem_matrix(path_table, nodes, waves, prefix)
  cross_lagged <- mat
  diag(cross_lagged) <- 0
  edge_table <- quicknet_edge_table(mat, directed = TRUE, drop_zero = FALSE, include_diag = TRUE)
  edge_table$edge_type <- ifelse(edge_table$from == edge_table$to, "autoregressive", "cross_lagged")
  node_table <- quicknet_directed_node_table(mat)

  quicknet_fit(
    model = "panel_sem",
    data = dat,
    networks = list(default = mat, cross_lagged = cross_lagged),
    edges = edge_table,
    nodes = node_table,
    fit = list(model = fit, fit_indices = quicknet_lavaan_fit_indices(fit), paths = path_table, syntax = syntax),
    meta = list(
      data_type = "panel",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "lavaan::sem",
      nodes = nodes,
      waves = waves,
      id = id,
      prefix = prefix,
      standardize = standardize,
      residual_cov = residual_cov,
      missing = missing,
      call = match.call()
    )
  )
}

quicknet_confirmatory_template <- function(omega, node_names) {
  if (is.null(omega)) {
    omega <- matrix(1, length(node_names), length(node_names), dimnames = list(node_names, node_names))
    diag(omega) <- 0
    return(omega)
  }
  omega <- as.matrix(omega)
  if (!all(dim(omega) == c(length(node_names), length(node_names)))) {
    stop("omega must have one row and one column per node.", call. = FALSE)
  }
  if (is.null(colnames(omega))) colnames(omega) <- node_names
  if (is.null(rownames(omega))) rownames(omega) <- node_names
  omega <- omega[node_names, node_names, drop = FALSE]
  omega[lower.tri(omega)] <- t(omega)[lower.tri(omega)]
  diag(omega) <- 0
  omega
}

quicknet_lavaan_fit_indices <- function(fit) {
  measures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  values <- lavaan::fitMeasures(fit, measures)
  data.frame(measure = names(values), value = as.numeric(values), row.names = NULL)
}

quicknet_lavaan_loadings <- function(fit) {
  solution <- lavaan::standardizedSolution(fit)
  solution[solution$op == "=~", intersect(c("lhs", "rhs", "est.std", "se", "z", "pvalue"), names(solution)), drop = FALSE]
}

quicknet_lavaan_residual_network <- function(fit, dat) {
  factor_scores <- as.data.frame(lavaan::lavPredict(fit))
  item_names <- colnames(dat)
  residual_matrix <- matrix(NA_real_, nrow = nrow(dat), ncol = length(item_names), dimnames = list(NULL, item_names))
  for (item in item_names) {
    regression_data <- cbind(item_value = dat[[item]], factor_scores)
    residual_matrix[, item] <- stats::resid(stats::lm(item_value ~ ., data = regression_data))
  }
  out <- stats::cor(residual_matrix, use = "pairwise.complete.obs")
  out <- quicknet_make_positive_definite(out)
  diag(out) <- 0
  colnames(out) <- rownames(out) <- item_names
  out
}

quicknet_panel_sem_syntax <- function(nodes, waves, prefix, residual_cov) {
  regression_lines <- character()
  for (wave_index in seq_len(length(waves) - 1)) {
    from_wave <- waves[[wave_index]]
    to_wave <- waves[[wave_index + 1]]
    predictors <- paste0(nodes, prefix, from_wave)
    for (node in nodes) {
      outcome <- paste0(node, prefix, to_wave)
      regression_lines <- c(regression_lines, paste(outcome, "~", paste(predictors, collapse = " + ")))
    }
  }
  residual_lines <- character()
  if (isTRUE(residual_cov)) {
    for (wave in waves[-1]) {
      wave_vars <- paste0(nodes, prefix, wave)
      for (i in seq_len(length(wave_vars) - 1)) {
        for (j in (i + 1):length(wave_vars)) {
          residual_lines <- c(residual_lines, paste(wave_vars[[i]], "~~", wave_vars[[j]]))
        }
      }
    }
  }
  paste(c(regression_lines, residual_lines), collapse = "\n")
}

quicknet_panel_sem_matrix <- function(path_table, nodes, waves, prefix) {
  mat <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  for (from_node in nodes) {
    for (to_node in nodes) {
      rows <- path_table[
        grepl(paste0("^", to_node, prefix, "(", paste(waves[-1], collapse = "|"), ")$"), path_table$lhs) &
          grepl(paste0("^", from_node, prefix, "(", paste(waves[-length(waves)], collapse = "|"), ")$"), path_table$rhs),
        ,
        drop = FALSE
      ]
      if (nrow(rows) > 0) {
        mat[to_node, from_node] <- mean(rows$est.std, na.rm = TRUE)
      }
    }
  }
  mat
}
