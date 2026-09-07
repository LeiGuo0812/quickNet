#' Estimate psychonetrics meta-analytic network models
#'
#' @param cors List of correlation matrices. Used by \code{"meta_ggm"} and
#' \code{"meta_cor"}.
#' @param covs List of covariance or Toeplitz covariance matrices.
#' @param nobs Sample sizes for \code{cors} or \code{covs}.
#' @param data Optional raw data. For \code{"meta_ggm"} and \code{"meta_cor"},
#' provide \code{studyvar}. For \code{"meta_gvar"}, provide \code{studyvar},
#' \code{id}, \code{day}, and \code{beep}.
#' @param studyvar Study identifier column when \code{data} is supplied.
#' @param vars Variables used as network nodes.
#' @param model Meta-analytic model. One of \code{"meta_ggm"},
#' \code{"meta_cor"}, or \code{"meta_gvar"}.
#' @param id Subject identifier for \code{"meta_gvar"} raw data.
#' @param day Day variable for \code{"meta_gvar"} raw data.
#' @param beep Measurement occasion variable for \code{"meta_gvar"} raw data.
#' @param randomEffects Random-effects structure passed to psychonetrics.
#' @param estimator Estimator passed to psychonetrics.
#' @param ... Additional arguments passed to the selected psychonetrics backend.
#'
#' @return A \code{quicknet_fit} object.
#' @export
MetaNet <- function(cors = NULL,
                    covs = NULL,
                    nobs = NULL,
                    data = NULL,
                    studyvar = NULL,
                    vars = NULL,
                    model = c("meta_ggm", "meta_cor", "meta_gvar"),
                    id = "id",
                    day = "day",
                    beep = "beep",
                    randomEffects = "chol",
                    estimator = "FIML",
                    ...) {
  model <- match.arg(model)
  vars <- quicknet_meta_infer_vars(
    vars = vars,
    cors = cors,
    covs = covs,
    data = data,
    studyvar = studyvar,
    id = id,
    day = day,
    beep = beep,
    model = model
  )
  quicknet_validate_input(
    data = data,
    model = model,
    cors = cors,
    covs = covs,
    nobs = nobs,
    studyvar = studyvar,
    vars = vars,
    id = id,
    day = day,
    beep = beep
  )
  if (!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("Package 'psychonetrics' is required for MetaNet().", call. = FALSE)
  }

  args <- quicknet_drop_nulls(list(
    data = data,
    cors = cors,
    covs = covs,
    nobs = nobs,
    studyvar = studyvar,
    vars = vars,
    randomEffects = randomEffects,
    estimator = estimator,
    verbose = FALSE,
    ...
  ))

  if (model == "meta_gvar") {
    args <- quicknet_drop_nulls(c(
      args,
      list(
        idvar = id,
        dayvar = day,
        beepvar = beep
      )
    ))
    raw_model <- suppressMessages(do.call(psychonetrics::meta_gvar, args))
  } else {
    args$type <- if (model == "meta_ggm") "ggm" else "cor"
    raw_model <- suppressMessages(do.call(psychonetrics::meta_varcov, args))
  }

  fit <- quicknet_psychonetrics_run(raw_model)
  networks <- if (model == "meta_gvar") {
    temporal <- quicknet_psychonetrics_matrix(fit, "beta", vars)
    contemporaneous <- quicknet_psychonetrics_first_matrix(fit, c("omega_zeta", "sigma_zeta", "kappa_zeta"), vars)
    list(default = temporal, temporal = temporal, contemporaneous = contemporaneous)
  } else {
    matrix_name <- if (model == "meta_ggm") "omega_y" else "rho_y"
    mat <- quicknet_psychonetrics_matrix(fit, matrix_name, vars)
    diag(mat) <- 0
    list(default = mat)
  }

  edges <- if (model == "meta_gvar") {
    quicknet_longitudinal_edges(networks)
  } else {
    quicknet_edge_table(networks$default, directed = FALSE, drop_zero = FALSE)
  }
  nodes <- if (model == "meta_gvar") {
    quicknet_bind_rows_fill(
      quicknet_directed_node_table(networks$temporal, network = "temporal"),
      quicknet_node_table(networks$contemporaneous, network = "contemporaneous")
    )
  } else {
    quicknet_node_table(networks$default)
  }

  quicknet_fit(
    model = model,
    data = data,
    networks = networks,
    edges = edges,
    nodes = nodes,
    fit = list(model = fit, fit_indices = quicknet_psychonetrics_fit_indices(fit)),
    meta = list(
      data_type = "meta",
      directed = model == "meta_gvar",
      row_is = if (model == "meta_gvar") "to" else NULL,
      col_is = if (model == "meta_gvar") "from" else NULL,
      backend = switch(
        model,
        meta_ggm = "psychonetrics::meta_varcov(type = 'ggm')",
        meta_cor = "psychonetrics::meta_varcov(type = 'cor')",
        meta_gvar = "psychonetrics::meta_gvar"
      ),
      vars = vars,
      studyvar = studyvar,
      id = if (model == "meta_gvar") id else NULL,
      day = if (model == "meta_gvar") day else NULL,
      beep = if (model == "meta_gvar") beep else NULL,
      n_studies = quicknet_meta_study_count(data, studyvar, cors, covs, nobs),
      nobs = nobs,
      randomEffects = randomEffects,
      estimator = estimator,
      call = match.call()
    )
  )
}

quicknet_drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

quicknet_meta_study_count <- function(data, studyvar, cors, covs, nobs) {
  if (!is.null(data) && !is.null(studyvar) && studyvar %in% colnames(data)) {
    return(length(unique(data[[studyvar]])))
  }
  if (!is.null(cors)) return(length(cors))
  if (!is.null(covs)) return(length(covs))
  if (!is.null(nobs)) return(length(nobs))
  NA_integer_
}

quicknet_meta_infer_vars <- function(vars, cors, covs, data, studyvar, id, day, beep, model) {
  if (!is.null(vars)) return(vars)
  if (!is.null(cors) && length(cors) > 0 && !is.null(colnames(cors[[1]]))) {
    return(colnames(cors[[1]]))
  }
  if (!is.null(covs) && length(covs) > 0 && !is.null(colnames(covs[[1]]))) {
    names <- colnames(covs[[1]])
    if (model == "meta_gvar" && length(names) %% 2 == 0) {
      return(names[seq_len(length(names) / 2)])
    }
    return(names)
  }
  matrices <- cors %||% covs
  if (is.list(matrices) && length(matrices) > 0 && is.matrix(matrices[[1]])) {
    p <- ncol(matrices[[1]])
    if (model == "meta_gvar") {
      if (p %% 2 != 0) {
        stop("Meta-GVAR covariance matrices must contain two equally sized variable blocks.", call. = FALSE)
      }
      p <- p / 2
    }
    return(paste0("V", seq_len(p)))
  }
  if (!is.null(data)) {
    excluded <- c(studyvar, id, day, beep)
    numeric <- vapply(data, is.numeric, logical(1))
    return(setdiff(names(data)[numeric], excluded))
  }
  vars
}
