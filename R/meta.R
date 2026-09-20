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
#' @details Named matrix row and column axes are independently aligned to vars.
#'   Named nobs are matched to unique study-list names; unnamed inputs are
#'   positional. Meta-GVAR matrices contain a past block followed by a current
#'   block, using the source tsData convention (var_lag1, then var). Variables
#'   can be reordered within either block. Raw Meta-GVAR data are sorted by
#'   study, subject, day and occasion; missing or duplicate time keys are rejected.
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
  aligned <- quicknet_meta_align_inputs(cors, covs, nobs, vars, model)
  cors <- aligned$cors
  covs <- aligned$covs
  nobs <- aligned$nobs
  if (!is.null(data)) {
    if (anyNA(data[[studyvar]])) stop("Study identifiers must not be missing.", call. = FALSE)
    if (model == "meta_gvar") {
      index_vars <- c(studyvar, id, day, beep)
      if (anyNA(data[, index_vars, drop = FALSE])) {
        stop("Study, subject, day and occasion identifiers must not be missing.", call. = FALSE)
      }
      if (anyDuplicated(data[, index_vars, drop = FALSE])) {
        stop("Each study/subject/day/occasion combination must be unique.", call. = FALSE)
      }
      data <- data[do.call(order, data[, index_vars, drop = FALSE]), , drop = FALSE]
    }
  }
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
    verbose = FALSE
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
    args <- quicknet_psychonetrics_args("meta_gvar", args, list(...))
    raw_model <- quicknet_capture_backend_warnings(suppressMessages(do.call(psychonetrics::meta_gvar, args)))
  } else {
    args$type <- if (model == "meta_ggm") "ggm" else "cor"
    args <- quicknet_psychonetrics_args("meta_varcov", args, list(...))
    raw_model <- quicknet_capture_backend_warnings(suppressMessages(do.call(psychonetrics::meta_varcov, args)))
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
      matrix_alignment = aligned$alignment,
      randomEffects = randomEffects,
      estimator = fit@estimator,
      backend_args = list(...),
      backend_settings = quicknet_psychonetrics_settings(fit, args),
      backend_version = as.character(utils::packageVersion("psychonetrics")),
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
      return(sub("_lag[0-9]+$", "", names[length(names) / 2 + seq_len(length(names) / 2)]))
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

# Study weights and matrix axes are independent labelled inputs. Normalize them
# before calling psychonetrics; the source estimator still performs estimation.
quicknet_meta_align_inputs <- function(cors, covs, nobs, vars, model) {
  if (!is.character(vars) || anyNA(vars) || anyDuplicated(vars) || !length(vars) || any(!nzchar(vars))) {
    stop("vars must contain unique, non-missing node names.", call. = FALSE)
  }
  if (!is.null(cors) && !is.null(covs)) stop("Supply cors or covs, not both.", call. = FALSE)
  matrices <- cors %||% covs
  if (is.null(matrices)) return(list(cors = cors, covs = covs, nobs = nobs, alignment = "raw data"))
  if (!is.null(names(nobs))) {
    if (is.null(names(matrices)) || anyNA(names(matrices)) || anyNA(names(nobs)) || anyDuplicated(names(matrices)) ||
        anyDuplicated(names(nobs)) || any(!nzchar(names(matrices))) ||
        !setequal(names(matrices), names(nobs))) {
      stop("Named nobs must match unique study matrix names.", call. = FALSE)
    }
    nobs <- nobs[match(names(matrices), names(nobs))]
  }
  matrices <- lapply(matrices, function(mat) {
    mat <- as.matrix(mat)
    p <- length(vars)
    expected <- if (model == "meta_gvar") 2L * p else p
    if (ncol(mat) != expected) stop("Study matrix dimensions do not match vars.", call. = FALSE)
    axis_order <- function(labels) {
      if (is.null(labels)) return(seq_len(expected))
      if (model == "meta_gvar") {
        # Native tsData uses past-variable_lag1 followed by current-variable.
        # Repeated names without suffixes are also unambiguous within each block.
        blocks <- split(seq_len(expected), rep(1:2, each = p))
        return(unlist(lapply(seq_along(blocks), function(k) {
          idx <- blocks[[k]]
          block_names <- labels[idx]
          # Match literal vars first: a real node may itself end in _lag1.
          alternatives <- list(vars, paste0(vars, if (k == 1L) "_lag1" else "_lag0"))
          for (names in alternatives) {
            if (!anyNA(block_names) && !anyDuplicated(block_names) && setequal(block_names, names)) {
              return(idx[match(names, block_names)])
            }
          }
          stop("Meta-GVAR time blocks must contain unique vars, with past (lag 1) followed by current (lag 0 or unsuffixed).", call. = FALSE)
        }), use.names = FALSE))
      }
      if (anyNA(labels) || anyDuplicated(labels) || !setequal(labels, vars)) {
        stop("Study matrix axis names must match vars.", call. = FALSE)
      }
      match(vars, labels)
    }
    mat <- mat[axis_order(rownames(mat)), axis_order(colnames(mat)), drop = FALSE]
    if (!is.numeric(mat) || any(is.infinite(mat)) ||
        !isTRUE(all.equal(unname(mat), unname(t(mat)), check.attributes = FALSE))) {
      stop("Study matrices must be numeric and symmetric after name alignment, without infinite values.", call. = FALSE)
    }
    labels <- if (model == "meta_gvar") c(paste0(vars, "_lag1"), vars) else vars
    dimnames(mat) <- list(labels, labels)
    mat
  })
  list(cors = if (!is.null(cors)) matrices else NULL,
       covs = if (!is.null(covs)) matrices else NULL, nobs = nobs,
       alignment = "Named axes aligned to vars; unnamed axes positional; named nobs aligned to study names.")
}
