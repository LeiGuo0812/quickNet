#' Estimate a confirmatory psychonetrics network model
#'
#' @param data Data frame containing network variables.
#' @param vars Variables used as nodes. Defaults to all columns.
#' @param omega Optional symmetric template matrix. Nonzero entries are freely
#' estimated and zero entries are fixed to zero. If \code{NULL}, all off-diagonal
#' edges are freely estimated.
#' @param estimator NULL inherits the psychonetrics estimator.
#' @param missing NULL inherits \code{"auto"} for covariance models and
#'   \code{"listwise"} for Ising.
#' @param model Confirmatory model. One of \code{"ggm"}, \code{"ising"},
#' \code{"cor"}, \code{"covariance"}, or \code{"precision"}.
#' @param sigma Optional covariance template for \code{model = "covariance"}.
#' @param kappa Optional precision template for \code{model = "precision"}.
#' @param rho Optional correlation template for \code{model = "cor"}.
#' @param tau Optional Ising threshold/intercept template.
#' @param beta Optional Ising inverse-temperature template.
#' @param beta_model Ising beta parameterization.
#' @param responses Optional response labels passed to \code{psychonetrics::Ising()}.
#' @param maxNodes Maximum number of Ising nodes passed to psychonetrics.
#' @param ... Additional arguments passed to the selected psychonetrics backend.
#'
#' @return A \code{quicknet_fit} object.
#' @export
ConfirmatoryNet <- function(data,
                            vars = NULL,
                            omega = NULL,
                            estimator = NULL,
                            missing = NULL,
                            model = c("ggm", "ising", "cor", "covariance", "precision"),
                            sigma = NULL,
                            kappa = NULL,
                            rho = NULL,
                            tau = NULL,
                            beta = NULL,
                            beta_model = c("beta", "log_beta"),
                            responses = NULL,
                            maxNodes = 20,
                            ...) {
  if (!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("Package 'psychonetrics' is required for ConfirmatoryNet().", call. = FALSE)
  }
  supplied <- names(match.call())
  model <- match.arg(model)
  if (model != "ising" && any(c("tau", "beta", "beta_model", "responses", "maxNodes") %in% supplied)) {
    stop("tau, beta, beta_model, responses and maxNodes apply only to confirmatory Ising models.", call. = FALSE)
  }
  beta_model <- match.arg(beta_model)
  source_fun <- if (model == "ising") psychonetrics::Ising else psychonetrics::varcov
  estimator <- estimator %||% quicknet_backend_default(source_fun, "estimator")
  missing <- missing %||% quicknet_backend_default(source_fun, "missing")
  model_key <- switch(
    model,
    ggm = "confirmatory_ggm",
    ising = "confirmatory_ising",
    cor = "confirmatory_cor",
    covariance = "confirmatory_covariance",
    precision = "confirmatory_precision"
  )
  quicknet_validate_input(
    data,
    model = model_key,
    vars = vars,
    omega = omega,
    sigma = sigma,
    kappa = kappa,
    rho = rho,
    missing = missing
  )
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  dat <- dat[, vars, drop = FALSE]
  dat <- quicknet_complete_numeric_data(dat, missing = quicknet_missing_mode(missing))
  node_names <- colnames(dat)
  fit <- quicknet_confirmatory_psychonetrics_fit(
    dat = dat,
    node_names = node_names,
    model = model,
    omega = omega,
    sigma = sigma,
    kappa = kappa,
    rho = rho,
    tau = tau,
    beta = beta,
    beta_model = beta_model,
    responses = responses,
    maxNodes = maxNodes,
    estimator = estimator,
    missing = missing,
    ...
  )
  mat <- as.matrix(psychonetrics::getmatrix(fit$model, fit$matrix_name))
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- node_names
  node_table <- quicknet_node_table(mat)
  if (model == "ising") {
    node_table <- quicknet_add_psychonetrics_ising_nodes(node_table, fit$model, dat)
  }

  quicknet_fit(
    model = model_key,
    data = dat,
    networks = list(default = mat),
    nodes = node_table,
    fit = list(model = fit$model, fit_indices = quicknet_psychonetrics_fit_indices(fit$model), template = fit$template),
    meta = list(
      data_type = "cross_sectional",
      directed = FALSE,
      backend = fit$backend,
      estimator = fit$model@estimator,
      missing = missing,
      backend_args = list(...),
      backend_settings = quicknet_psychonetrics_settings(fit$model, fit$args),
      backend_version = as.character(utils::packageVersion("psychonetrics")),
      beta_model = if (model == "ising") beta_model else NULL,
      maxNodes = if (model == "ising") maxNodes else NULL,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    )
  )
}

quicknet_confirmatory_psychonetrics_fit <- function(dat,
                                                    node_names,
                                                    model,
                                                    omega,
                                                    sigma,
                                                    kappa,
                                                    rho,
                                                    tau,
                                                    beta,
                                                    beta_model,
                                                    responses,
                                                    maxNodes,
                                                    estimator,
                                                    missing,
                                                    ...) {
  matrix_name <- switch(model, ggm = "omega", ising = "omega", cor = "rho", covariance = "sigma", precision = "kappa")
  supplied <- list(omega = omega, sigma = sigma, kappa = kappa, rho = rho)
  ignored <- setdiff(names(supplied)[!vapply(supplied, is.null, logical(1))], matrix_name)
  if (length(ignored)) stop("Template argument(s) do not apply to this model: ", paste(ignored, collapse = ", "), call. = FALSE)
  template <- quicknet_confirmatory_template(supplied[[matrix_name]], node_names,
    diag_value = if (matrix_name == "omega") 0 else 1)
  fun <- if (model == "ising") "Ising" else if (model == "ggm") "ggm" else "varcov"
  args <- list(data = dat, vars = node_names, estimator = estimator, missing = missing, verbose = FALSE)
  args[[matrix_name]] <- template
  if (model == "ising") {
    args <- c(args, quicknet_drop_nulls(list(tau = tau, beta = beta, beta_model = beta_model,
      responses = responses, maxNodes = maxNodes)))
  } else if (model != "ggm") args$type <- switch(model, cor = "cor", covariance = "cov", precision = "prec")
  args <- quicknet_psychonetrics_args(fun, args, list(...))
  raw <- quicknet_capture_backend_warnings(do.call(get(fun, asNamespace("psychonetrics")), args))
  list(model = quicknet_psychonetrics_run(raw), matrix_name = matrix_name, template = template,
       backend = paste0("psychonetrics::", fun), args = args)
}

quicknet_add_psychonetrics_ising_nodes <- function(node_table, model, dat) {
  pred <- quicknet_binary_predictability(dat)
  node_table <- merge(node_table, pred, by = "node", all.x = TRUE, sort = FALSE)
  thresholds <- tryCatch(
    as.numeric(psychonetrics::getmatrix(model, "tau")),
    error = function(e) NULL
  )
  if (!is.null(thresholds) && length(thresholds) == nrow(node_table)) {
    node_table$threshold <- thresholds[match(node_table$node, colnames(dat))]
  }
  node_table
}

#' Estimate latent and residual networks
#'
#' @param data Data frame containing manifest variables.
#' @param model Lavaan CFA model syntax, or one of \code{"lvm"},
#' \code{"lnm"}, \code{"rnm"}, or \code{"lrnm"} for psychonetrics backends.
#' @param vars Manifest variables used in the CFA. Defaults to variables found
#' in \code{data}.
#' @param std.lv lavaan identification control; NULL inherits FALSE.
#' @param missing NULL inherits the backend missing-data rule: listwise for
#'   lavaan and auto for psychonetrics.
#' @param residual For lavaan, NULL returns the supplementary factor-score
#'   residual correlation network; FALSE omits it. For psychonetrics lvm,
#'   an optional native residual covariance parameterization. Named network
#'   aliases fix their parameterization.
#' @param lambda Factor loading matrix used by psychonetrics latent-variable
#' models.
#' @param latents Optional latent variable names used by psychonetrics.
#' @param estimator Estimator passed to the selected lavaan or psychonetrics
#'   backend; NULL inherits its default, including ordered-data behavior.
#' @param identification Identification method passed to
#' \code{psychonetrics::lvm()}.
#' @param ... Additional arguments passed to the selected backend.
#'
#' @return A \code{quicknet_fit} object with model \code{"latent_network"}.
#' @export
LatentNet <- function(data,
                      model,
                      vars = NULL,
                      std.lv = NULL,
                      missing = NULL,
                      residual = NULL,
                      lambda = NULL,
                      latents = NULL,
                      estimator = NULL,
                      identification = c("loadings", "variance"),
                      ...) {
  psychonetrics_models <- c("lvm", "lnm", "rnm", "lrnm")
  if (length(model) == 1 && model %in% psychonetrics_models) {
    if (!is.null(std.lv)) stop("std.lv applies only to the lavaan backend.", call. = FALSE)
    if (is.logical(residual)) stop("For psychonetrics, residual must be a covariance parameterization, not a logical value.", call. = FALSE)
    estimator <- estimator %||% "default"
    missing <- missing %||% "auto"
    identification <- match.arg(identification)
    quicknet_validate_input(data, model = model, vars = vars, lambda = lambda, missing = missing)
    return(quicknet_psychonetrics_latent_fit(
      data = data,
      model = model,
      vars = vars,
      lambda = lambda,
      latents = latents,
      missing = missing,
      estimator = estimator,
      identification = identification,
      residual = residual,
      call = match.call(),
      ...
    ))
  }
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required for LatentNet().", call. = FALSE)
  }
  if (!is.null(lambda) || !is.null(latents) || !missing(identification)) stop("lambda, latents and identification apply only to psychonetrics latent models.", call. = FALSE)
  missing <- missing %||% "listwise"
  std.lv <- std.lv %||% FALSE
  residual <- residual %||% TRUE
  quicknet_validate_input(
    data,
    model = "latent_network",
    vars = vars,
    syntax = model,
    missing = missing
  )
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  dat <- dat[, vars, drop = FALSE]
  dat <- quicknet_complete_numeric_data(dat, missing = quicknet_missing_mode(missing))

  args <- quicknet_backend_args(list(...), lavaan::cfa,
    reserved = c("model", "data", "std.lv", "missing", "estimator"), extra = names(lavaan::lavOptions()))
  args <- c(list(std.lv = std.lv, missing = missing), if (!is.null(estimator)) list(estimator = estimator), args)
  fit <- quicknet_capture_backend_warnings(do.call(lavaan::cfa, c(list(model = model, data = dat), args)))
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
      backend_version = as.character(utils::packageVersion("lavaan")),
      backend_args = args,
      backend_settings = fit@Options,
      estimator = quicknet_lavaan_estimator(fit),
      std.lv = fit@Options$std.lv,
      missing = fit@Options$missing,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    )
  )
}

quicknet_psychonetrics_latent_fit <- function(data,
                                              model,
                                              vars,
                                              lambda,
                                              latents,
                                              missing,
                                              estimator,
                                              identification,
                                              residual,
                                              call,
                                              ...) {
  if (!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("Package 'psychonetrics' is required for LatentNet(model = '", model, "').", call. = FALSE)
  }
  dat <- as.data.frame(data)
  if (is.null(vars)) vars <- colnames(dat)
  missing_cols <- setdiff(vars, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  dat <- dat[, vars, drop = FALSE]
  dat <- quicknet_complete_numeric_data(dat, missing = quicknet_missing_mode(missing))
  lambda <- as.matrix(lambda)
  if (is.null(rownames(lambda))) rownames(lambda) <- vars
  if (is.null(latents)) {
    latents <- colnames(lambda) %||% paste0("F", seq_len(ncol(lambda)))
  }
  if (is.null(colnames(lambda))) colnames(lambda) <- latents
  lambda <- lambda[vars, latents, drop = FALSE]

  args <- list(data = dat, lambda = lambda, vars = vars, latents = latents, missing = missing,
    estimator = estimator, identification = identification, verbose = FALSE)
  if (!is.null(residual)) args$residual <- residual
  args <- quicknet_psychonetrics_args(model, args, list(...))
  raw_model <- quicknet_capture_backend_warnings(do.call(get(model, asNamespace("psychonetrics")), args))
  fit <- quicknet_psychonetrics_run(raw_model)

  networks <- quicknet_psychonetrics_latent_networks(fit, model, vars = vars, latents = latents)
  node_tables <- lapply(names(networks), function(network_name) {
    quicknet_node_table(networks[[network_name]], network = network_name)
  })
  loadings <- quicknet_psychonetrics_loadings(fit)

  quicknet_fit(
    model = model,
    data = dat,
    networks = networks,
    nodes = do.call(quicknet_bind_rows_fill, node_tables),
    fit = list(
      model = fit,
      fit_indices = quicknet_psychonetrics_fit_indices(fit),
      loadings = loadings,
      lambda = lambda
    ),
    meta = list(
      data_type = "latent",
      directed = FALSE,
      backend = paste0("psychonetrics::", model),
      vars = vars,
      latents = latents,
      missing = missing,
      estimator = fit@estimator,
      identification = fit@identification,
      backend_args = list(...),
      backend_settings = quicknet_psychonetrics_settings(fit, args),
      backend_version = as.character(utils::packageVersion("psychonetrics")),
      n = nrow(dat),
      p = ncol(dat),
      call = call
    )
  )
}

quicknet_psychonetrics_latent_networks <- function(fit, model, vars, latents) {
  latent_candidates <- if (model %in% c("lnm", "lrnm")) {
    c("omega_zeta", "sigma_zeta", "kappa_zeta")
  } else {
    c("sigma_zeta", "omega_zeta", "kappa_zeta")
  }
  residual_candidates <- if (model %in% c("rnm", "lrnm")) {
    c("omega_epsilon", "sigma_epsilon", "kappa_epsilon")
  } else {
    c("sigma_epsilon", "omega_epsilon", "kappa_epsilon")
  }
  latent <- quicknet_psychonetrics_first_matrix(fit, latent_candidates, latents)
  residual <- quicknet_psychonetrics_first_matrix(fit, residual_candidates, vars)
  networks <- if (model == "rnm") {
    list(default = residual, residual = residual, latent = latent)
  } else {
    list(default = latent, latent = latent)
  }
  if (model %in% c("rnm", "lrnm", "lvm")) {
    networks$residual <- residual
  }
  networks
}

quicknet_psychonetrics_loadings <- function(fit) {
  pars <- data.frame()
  invisible(utils::capture.output({
    pars <- tryCatch(psychonetrics::parameters(fit), error = function(e) data.frame())
  }))
  if (!nrow(pars) || !"matrix" %in% names(pars)) return(data.frame())
  pars[pars$matrix == "lambda", intersect(c("var1", "op", "var2", "est", "se", "p", "fixed"), names(pars)), drop = FALSE]
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
#' @param ... Additional named lavaan controls, e.g. \code{estimator = "MLR"}.
#'   The generated panel model fixes auto.cov.y = FALSE so residual_cov
#'   and the explicit covariance syntax determine residual connections.
#'
#' @return A \code{quicknet_fit} object with model \code{"panel_sem"}.
#' @export
PanelSEMNet <- function(data,
                        nodes,
                        waves,
                        id = "id",
                        prefix = "_t",
                        standardize = FALSE,
                        residual_cov = TRUE,
                        missing = "listwise",
                        ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required for PanelSEMNet().", call. = FALSE)
  }
  quicknet_validate_input(
    data,
    model = "panel_sem",
    nodes = nodes,
    waves = waves,
    id = id,
    prefix = prefix,
    missing = missing
  )
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
  if (identical(quicknet_missing_mode(missing), "listwise")) {
    dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  }
  if (standardize) {
    dat[required_columns] <- lapply(dat[required_columns], function(x) as.numeric(scale(x)))
  }

  syntax <- quicknet_panel_sem_syntax(nodes, waves, prefix, residual_cov = residual_cov)
  args <- quicknet_backend_args(list(...), lavaan::sem,
    reserved = c("model", "data", "missing", "auto.cov.y"), extra = names(lavaan::lavOptions()))
  args <- quicknet_merge_args(list(missing = missing, auto.cov.y = FALSE), args)
  fit <- quicknet_capture_backend_warnings(do.call(lavaan::sem, c(list(model = syntax, data = dat), args)))
  parameters <- lavaan::standardizedSolution(fit)
  path_table <- parameters[parameters$op == "~", c("lhs", "rhs", "est.std", "se", "z", "pvalue")]
  mat <- quicknet_panel_sem_matrix(path_table, nodes, waves, prefix)
  cross_lagged <- mat
  diag(cross_lagged) <- 0
  networks <- list(default = mat, cross_lagged = cross_lagged)
  edge_table <- quicknet_edge_table(
    mat,
    directed = TRUE,
    drop_zero = FALSE,
    include_diag = TRUE
  )
  edge_table$edge_type <- ifelse(
    edge_table$from == edge_table$to,
    "autoregressive",
    "cross_lagged"
  )
  node_tables <- list(quicknet_directed_node_table(mat))
  residual_paths <- parameters[
    parameters$op == "~~" & parameters$lhs != parameters$rhs,
    c("lhs", "rhs", "est.std", "se", "z", "pvalue"),
    drop = FALSE
  ]
  contemporaneous <- NULL
  if (isTRUE(residual_cov)) {
    contemporaneous <- quicknet_panel_sem_contemporaneous(
      residual_paths,
      nodes,
      waves,
      prefix
    )
    networks$contemporaneous <- contemporaneous
    contemporaneous_edges <- quicknet_edge_table(
      contemporaneous,
      network = "contemporaneous",
      directed = FALSE,
      drop_zero = FALSE
    )
    contemporaneous_edges$edge_type <- "contemporaneous"
    edge_table <- quicknet_bind_rows_fill(edge_table, contemporaneous_edges)
    node_tables[[length(node_tables) + 1]] <-
      quicknet_node_table(contemporaneous, network = "contemporaneous")
  }

  quicknet_fit(
    model = "panel_sem",
    data = dat,
    networks = networks,
    edges = edge_table,
    nodes = do.call(quicknet_bind_rows_fill, node_tables),
    fit = list(
      model = fit,
      fit_indices = quicknet_lavaan_fit_indices(fit),
      paths = path_table,
      residual_paths = residual_paths,
      syntax = syntax
    ),
    meta = list(
      data_type = "panel",
      directed = TRUE,
      row_is = "to",
      col_is = "from",
      backend = "lavaan::sem",
      backend_version = as.character(utils::packageVersion("lavaan")),
      backend_args = args,
      backend_settings = fit@Options,
      estimator = quicknet_lavaan_estimator(fit),
      method_presets = list(auto.cov.y = FALSE, syntax = "adjacent-wave regressions and explicit residual_cov constraints"),
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

quicknet_confirmatory_template <- function(omega, node_names, diag_value = 0) {
  if (is.null(omega)) {
    omega <- matrix(1, length(node_names), length(node_names), dimnames = list(node_names, node_names))
    diag(omega) <- diag_value
    return(omega)
  }
  omega <- as.matrix(omega)
  if (!all(dim(omega) == c(length(node_names), length(node_names)))) {
    stop("omega must have one row and one column per node.", call. = FALSE)
  }
  if (!is.numeric(omega) || any(!is.finite(omega))) {
    stop("The network template must contain finite numeric values.", call. = FALSE)
  }
  if (is.null(colnames(omega))) colnames(omega) <- node_names
  if (is.null(rownames(omega))) rownames(omega) <- node_names
  if (anyDuplicated(rownames(omega)) || anyDuplicated(colnames(omega)) ||
      !setequal(rownames(omega), node_names) || !setequal(colnames(omega), node_names)) {
    stop("The network template names must match the node names.", call. = FALSE)
  }
  omega <- omega[node_names, node_names, drop = FALSE]
  if (!isSymmetric(omega)) {
    stop("The network template must be symmetric.", call. = FALSE)
  }
  diag(omega) <- diag_value
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
    residual_matrix[, item] <- stats::resid(stats::lm(
      item_value ~ .,
      data = regression_data,
      na.action = stats::na.exclude
    ))
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
      expected_lhs <- paste0(to_node, prefix, waves[-1])
      expected_rhs <- paste0(from_node, prefix, waves[-length(waves)])
      rows <- path_table[path_table$lhs %in% expected_lhs & path_table$rhs %in% expected_rhs, , drop = FALSE]
      if (nrow(rows) > 0) {
        mat[to_node, from_node] <- mean(rows$est.std, na.rm = TRUE)
      }
    }
  }
  mat
}

quicknet_panel_sem_contemporaneous <- function(residual_paths, nodes, waves, prefix) {
  mat <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  if (nrow(residual_paths) == 0 || length(nodes) < 2) return(mat)

  residual_waves <- waves[-1]
  for (node_i_index in seq_len(length(nodes) - 1)) {
    for (node_j_index in seq.int(node_i_index + 1, length(nodes))) {
      node_i <- nodes[[node_i_index]]
      node_j <- nodes[[node_j_index]]
      values <- numeric()
      for (wave in residual_waves) {
        name_i <- paste0(node_i, prefix, wave)
        name_j <- paste0(node_j, prefix, wave)
        rows <- residual_paths[
          (residual_paths$lhs == name_i & residual_paths$rhs == name_j) |
            (residual_paths$lhs == name_j & residual_paths$rhs == name_i),
          ,
          drop = FALSE
        ]
        if (nrow(rows) > 0) values <- c(values, rows$est.std)
      }
      value <- quicknet_safe_mean(values)
      mat[node_i, node_j] <- mat[node_j, node_i] <- value
    }
  }
  mat
}
