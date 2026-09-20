#' Create a quickNet fit object
#'
#' @param model Model name.
#' @param data Original input data.
#' @param networks Nonempty, uniquely named list of finite numeric square network
#'   matrices. Node names, when supplied, must be unique; rows are aligned to
#'   column order. The first layer is used when no layer is named "default".
#' @param edges Edge table.
#' @param nodes Node-level table.
#' @param fit Raw model object returned by the backend package.
#' @param plots Named list of plot objects.
#' @param meta Named list of model metadata.
#' @param network_summary Network-level summary table.
#'
#' @return A \code{quicknet_fit} object.
#' @export
quicknet_fit <- function(model,
                         data = NULL,
                         networks,
                         edges = NULL,
                         nodes = NULL,
                         fit = NULL,
                         plots = list(),
                         meta = list(),
                         network_summary = NULL) {
  if (!is.list(networks) || length(networks) == 0L || is.null(names(networks)) ||
      anyNA(names(networks)) || any(names(networks) == "") || anyDuplicated(names(networks))) {
    stop("networks must be a named list of matrices.", call. = FALSE)
  }

  networks <- lapply(networks, function(network) {
    network <- as.matrix(network)
    if (!is.numeric(network) || nrow(network) == 0L || nrow(network) != ncol(network) ||
        any(!is.finite(network))) {
      stop("Each network must be a nonempty, finite numeric square matrix.", call. = FALSE)
    }
    rn <- rownames(network)
    cn <- colnames(network)
    for (nm in list(rn, cn)) {
      if (!is.null(nm) && (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))) {
        stop("Network node names must be unique and non-missing.", call. = FALSE)
      }
    }
    if (!is.null(rn) && !is.null(cn) && !identical(rn, cn)) {
      if (!setequal(rn, cn)) stop("Network row and column names must identify the same nodes.", call. = FALSE)
      network <- network[cn, cn, drop = FALSE]
    }
    node_names <- cn %||% rn %||% paste0("V", seq_len(ncol(network)))
    dimnames(network) <- list(node_names, node_names)
    storage.mode(network) <- "double"
    network
  })
  if ("default" %in% names(networks)) {
    networks <- networks[c("default", setdiff(names(networks), "default"))]
  }

  default_network <- networks[[1]]
  default_directed <- quicknet_network_summary_is_directed(model, meta, names(networks)[[1]])
  edge_table <- if (is.null(edges)) {
    quicknet_edge_table(default_network, directed = default_directed)
  } else {
    edges
  }
  node_table <- if (is.null(nodes)) {
    if (default_directed) quicknet_directed_node_table(default_network) else quicknet_node_table(default_network)
  } else {
    nodes
  }
  summary_table <- if (is.null(network_summary)) {
    quicknet_network_summary_list(networks, model = model, meta = meta)
  } else {
    network_summary
  }
  diagnostics <- quicknet_backend_diagnostics(fit)
  meta$analysis_sample <- quicknet_analysis_sample(data, fit, meta, model)
  if (any(diagnostics$status == "failed")) {
    warning("The backend reported an unsuccessful or inadmissible fit; inspect diagnostics before interpreting its estimates.", call. = FALSE)
  } else if (any(diagnostics$status == "partial")) {
    warning("glmnet returned a partial regularization path; inspect diagnostics and the available lambda range.", call. = FALSE)
  }

  structure(
    list(
      model = model,
      data = data,
      networks = networks,
      edges = edge_table,
      nodes = node_table,
      fit = fit,
      plots = plots,
      meta = meta,
      diagnostics = diagnostics,
      network_summary = summary_table,
      graph = default_network,
      graphData = list(graph = default_network),
      Edgelist = quicknet_edgelist(default_network, directed = default_directed)
    ),
    class = "quicknet_fit"
  )
}

#' @export
print.quicknet_fit <- function(x, ...) {
  cat("<quicknet_fit>\n")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Networks: ", paste(names(x$networks), collapse = ", "), "\n", sep = "")
  cat("Nodes: ", ncol(x$networks[[1]]), "\n", sep = "")
  nonzero_edges <- if (!is.null(x$network_summary$nonzero_edges)) {
    x$network_summary$nonzero_edges[[1]]
  } else {
    sum(abs(x$networks[[1]][upper.tri(x$networks[[1]])]) > 1e-10, na.rm = TRUE)
  }
  cat("Nonzero edges: ", nonzero_edges, "\n", sep = "")
  issue <- quicknet_fit_failure_reason(x)
  if (!is.null(issue)) cat("Fit status: unsuccessful. ", issue, "\n", sep = "")
  if (any(quicknet_fit_diagnostics(x)$status == "partial")) cat("Fit status: partial regularization path; inspect diagnostics.\n")
  explanation <- quicknet_ising_comparison_notes(x$model, quicknet_fit_gamma(x))
  quicknet_print_comparison_notes(explanation)
  if (length(explanation) > 0L) cat("Reference: ", quicknet_nira_reference(), "\n", sep = "")
  invisible(x)
}

#' @export
summary.quicknet_fit <- function(object, ...) {
  object$network_summary
}

#' @export
plot.quicknet_fit <- function(x, network = "default", ...) {
  mat <- quicknet_network_matrix(x, network = network)
  directed <- quicknet_is_directed(x, network)
  args <- list(...)
  if (identical(network, "default") && inherits(x$plots$network, "qgraph")) {
    defaults <- x$plots$network$Arguments
    defaults$input <- NULL
    defaults$DoNotPlot <- FALSE
    defaults$layout <- x$plots$network$layout
    args <- utils::modifyList(defaults, args, keep.null = TRUE)
  }
  if (is.null(args$directed)) args$directed <- directed
  do.call(
    qgraph::qgraph,
    c(list(quicknet_to_qgraph_matrix(mat, directed = directed)), args)
  )
}

quicknet_network_summary <- function(weight_matrix, threshold = 1e-10, directed = FALSE) {
  mat <- as.matrix(weight_matrix)
  diag(mat) <- 0
  values <- if (directed) mat[row(mat) != col(mat)] else mat[upper.tri(mat)]
  nonzero <- abs(values) > threshold
  data.frame(
    nodes = ncol(mat),
    possible_edges = length(values),
    nonzero_edges = sum(nonzero, na.rm = TRUE),
    density = if (length(nonzero) > 0) mean(nonzero, na.rm = TRUE) else 0,
    mean_abs_weight = ifelse(any(nonzero, na.rm = TRUE), mean(abs(values[nonzero]), na.rm = TRUE), 0),
    max_abs_weight = ifelse(any(nonzero, na.rm = TRUE), max(abs(values[nonzero]), na.rm = TRUE), 0),
    directed = directed,
    stringsAsFactors = FALSE
  )
}

quicknet_network_summary_list <- function(networks, model, meta, threshold = 1e-10) {
  rows <- lapply(names(networks), function(network_name) {
    directed <- quicknet_network_summary_is_directed(model, meta, network_name)
    summary <- quicknet_network_summary(networks[[network_name]], threshold = threshold, directed = directed)
    summary$network <- network_name
    summary[, c("network", setdiff(names(summary), "network")), drop = FALSE]
  })
  do.call(rbind, rows)
}

quicknet_network_summary_is_directed <- function(model, meta, network_name) {
  if (model %in% c("clpn", "mixedVAR", "time_varying_mvar")) return(TRUE)
  if (model == "panel_sem") {
    return(network_name %in% c("default", "cross_lagged"))
  }
  if (model %in% c("graphicalVAR", "mlVAR", "psychonetrics_gvar", "ri_clpm", "panel_gvar", "panel_var", "meta_gvar")) {
    return(quicknet_longitudinal_network_is_directed(network_name))
  }
  isTRUE(meta$directed)
}

quicknet_network_matrix <- function(x, network = "default") {
  if (inherits(x, "quicknet_fit")) {
    if (identical(network, "default") && !"default" %in% names(x$networks)) {
      network <- names(x$networks)[[1L]]
    }
    if (!network %in% names(x$networks)) {
      stop("Unknown network: ", network, call. = FALSE)
    }
    return(x$networks[[network]])
  }

  if (inherits(x, "qgraph")) {
    return(quicknet_from_qgraph_matrix(qgraph::getWmat(x), directed = quicknet_is_directed(x)))
  }

  if (is.matrix(x) || is.data.frame(x)) {
    mat <- as.matrix(x)
    if (!is.numeric(mat) || nrow(mat) != ncol(mat) || !nrow(mat) || any(!is.finite(mat))) {
      stop("A network matrix must be a nonempty finite numeric square matrix.", call. = FALSE)
    }
    rn <- rownames(mat)
    cn <- colnames(mat)
    if (!is.null(rn) && !is.null(cn)) {
      if (anyNA(rn) || anyNA(cn) || anyDuplicated(rn) || anyDuplicated(cn) || !setequal(rn, cn)) {
        stop("Network row and column names must identify the same unique nodes.", call. = FALSE)
      }
      mat <- mat[match(cn, rn), , drop = FALSE]
    }
    return(mat)
  }

  if (is.list(x) && !is.null(x[["graph", exact = TRUE]])) {
    return(as.matrix(x[["graph", exact = TRUE]]))
  }

  stop("Cannot extract a network matrix from this object.", call. = FALSE)
}

quicknet_edge_table <- function(weight_matrix,
                                network = "default",
                                directed = FALSE,
                                drop_zero = FALSE,
                                include_diag = FALSE,
                                threshold = 1e-10) {
  mat <- as.matrix(weight_matrix)
  node_names <- colnames(mat)
  if (is.null(node_names)) node_names <- rownames(mat)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(ncol(mat)))

  if (directed) {
    index <- which(if (include_diag) row(mat) == row(mat) else row(mat) != col(mat), arr.ind = TRUE)
  } else {
    index <- which(upper.tri(mat), arr.ind = TRUE)
  }

  out <- data.frame(
    network = rep(network, nrow(index)),
    from = node_names[index[, "col"]],
    to = node_names[index[, "row"]],
    from_index = index[, "col"],
    to_index = index[, "row"],
    weight = mat[index],
    directed = rep(directed, nrow(index)),
    stringsAsFactors = FALSE
  )
  out$abs_weight <- abs(out$weight)
  out$sign <- ifelse(out$weight > 0, "positive", ifelse(out$weight < 0, "negative", "zero"))

  if (drop_zero) {
    out <- out[out$abs_weight > threshold, , drop = FALSE]
  }

  out[order(-out$abs_weight, out$from_index, out$to_index), , drop = FALSE]
}

quicknet_edgelist <- function(weight_matrix, threshold = 1e-10, directed = FALSE, include_diag = FALSE) {
  edges <- quicknet_edge_table(
    weight_matrix,
    directed = directed,
    include_diag = include_diag,
    drop_zero = TRUE,
    threshold = threshold
  )
  data.frame(
    from = edges$from_index,
    to = edges$to_index,
    weight = edges$weight
  )
}

quicknet_node_table <- function(weight_matrix, network = "default", threshold = 1e-10) {
  mat <- as.matrix(weight_matrix)
  diag(mat) <- 0
  node_names <- colnames(mat)
  if (is.null(node_names)) node_names <- rownames(mat)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(ncol(mat)))

  data.frame(
    network = network,
    node = node_names,
    strength = rowSums(abs(mat), na.rm = TRUE),
    expected_influence = rowSums(mat, na.rm = TRUE),
    degree_nonzero = rowSums(abs(mat) > threshold, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

quicknet_directed_node_table <- function(weight_matrix, network = "default", threshold = 1e-10) {
  mat <- as.matrix(weight_matrix)
  node_names <- rownames(mat)
  if (is.null(node_names)) node_names <- colnames(mat)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(nrow(mat)))
  cross_mat <- mat
  diag(cross_mat) <- 0
  data.frame(
    network = network,
    node = node_names,
    autoregressive = diag(mat),
    in_strength = rowSums(abs(cross_mat), na.rm = TRUE),
    out_strength = colSums(abs(cross_mat), na.rm = TRUE),
    in_expected_influence = rowSums(cross_mat, na.rm = TRUE),
    out_expected_influence = colSums(cross_mat, na.rm = TRUE),
    in_degree_nonzero = rowSums(abs(cross_mat) > threshold, na.rm = TRUE),
    out_degree_nonzero = colSums(abs(cross_mat) > threshold, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

quicknet_continuous_predictability <- function(data) {
  dat <- as.data.frame(data)
  out <- data.frame(node = colnames(dat), predictability_R2 = NA_real_)
  for (target in colnames(dat)) {
    predictors <- setdiff(colnames(dat), target)
    model_data <- dat[, c(target, predictors), drop = FALSE]
    model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]
    if (nrow(model_data) < length(predictors) + 5) next
    fit <- stats::lm(quicknet_additive_formula(target, predictors), data = model_data)
    out$predictability_R2[out$node == target] <- summary(fit)$r.squared
  }
  out
}

quicknet_binary_predictability <- function(data) {
  dat <- as.data.frame(data)
  out <- data.frame(
    node = colnames(dat),
    prevalence = NA_real_,
    accuracy = NA_real_,
    null_accuracy = NA_real_,
    accuracy_gain = NA_real_
  )
  for (target in colnames(dat)) {
    predictors <- setdiff(colnames(dat), target)
    model_data <- dat[, c(target, predictors), drop = FALSE]
    model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]
    if (nrow(model_data) < length(predictors) + 10 || length(unique(model_data[[target]])) != 2) next
    fit <- tryCatch(
      stats::glm(
        quicknet_additive_formula(target, predictors),
        data = model_data,
        family = stats::binomial()
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    observed <- as.integer(model_data[[target]])
    predicted <- ifelse(stats::predict(fit, type = "response") >= 0.5, 1L, 0L)
    prevalence <- mean(observed == 1)
    accuracy <- mean(predicted == observed)
    null_accuracy <- max(prevalence, 1 - prevalence)
    out[out$node == target, c("prevalence", "accuracy", "null_accuracy", "accuracy_gain")] <- c(
      prevalence,
      accuracy,
      null_accuracy,
      accuracy - null_accuracy
    )
  }
  out
}

quicknet_complete_numeric_data <- function(data, missing = c("listwise", "none")) {
  missing <- match.arg(missing)
  dat <- as.data.frame(data)
  numeric_cols <- vapply(dat, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("All selected network variables must be numeric for this model.", call. = FALSE)
  }
  if (missing == "listwise") {
    dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  }
  if (nrow(dat) < 3) {
    stop("At least 3 complete observations are required.", call. = FALSE)
  }
  dat
}

quicknet_make_positive_definite <- function(mat) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  mat <- (mat + t(mat)) / 2
  diag(mat) <- 1

  eigen_values <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigen_values) > 1e-8) return(mat)

  adjusted <- as.matrix(Matrix::nearPD(mat, corr = TRUE)$mat)
  diag(adjusted) <- 1
  adjusted
}

quicknet_partial_cor <- function(correlation_matrix) {
  precision <- solve(as.matrix(correlation_matrix))
  partial <- -stats::cov2cor(precision)
  diag(partial) <- 0
  colnames(partial) <- rownames(partial) <- colnames(correlation_matrix)
  partial
}

quicknet_fit_cross_sectional <- function(data,
                                         model = c("EBICglasso", "correlation", "partial", "ising", "ordinal", "mgm"),
                                         cor_method = NULL,
                                         missing = NULL,
                                         gamma = NULL,
                                         ordinal_method = c("polychoric", "spearman", "pearson"),
                                         AND = TRUE,
                                         types = NULL,
                                         levels = NULL,
                                         backend_args = list(),
                                         repair_pd = FALSE) {
  model <- match.arg(model)
  if (!is.null(cor_method)) cor_method <- match.arg(cor_method, c("pearson", "spearman", "kendall"))
  missing <- quicknet_cross_missing(model, missing %||% if (model == "EBICglasso") backend_args$missing else NULL)
  ordinal_method <- match.arg(ordinal_method)
  dat <- quicknet_complete_numeric_data(data, missing = if (missing == "listwise") "listwise" else "none")
  if (missing == "stop" && anyNA(dat)) stop("Missing data detected.", call. = FALSE)
  node_names <- colnames(dat)
  selection <- NULL
  provenance <- list()

  if (model == "EBICglasso") {
    args <- quicknet_backend_args(backend_args, bootnet::bootnet_EBICglasso,
      reserved = c("data", "unlock"), extra = setdiff(names(formals(qgraph::EBICglasso)), c("S", "n", "gamma", "...")))
    if (!is.null(gamma) && !is.null(args$tuning) && !identical(as.numeric(gamma), as.numeric(args$tuning))) {
      stop("gamma and tuning conflict.", call. = FALSE)
    }
    gamma <- quicknet_resolve_gamma(model, gamma %||% args$tuning)
    if (!is.null(cor_method)) {
      if (!is.null(args$corMethod) && args$corMethod != "cor") stop("cor_method requires corMethod = 'cor'.", call. = FALSE)
      if (!is.null(args$corArgs$method) && args$corArgs$method != cor_method) stop("cor_method and corArgs$method conflict.", call. = FALSE)
      args$corMethod <- "cor"
      args$corArgs <- quicknet_merge_args(args$corArgs %||% list(), list(method = cor_method))
    }
    if (!is.null(args$missing) && args$missing != missing) stop("Specify missing through the missing argument.", call. = FALSE)
    args <- quicknet_merge_args(list(verbose = FALSE), args)
    args$tuning <- gamma
    args$missing <- if (missing == "none") "stop" else missing
    fit <- do.call(bootnet::estimateNetwork, c(list(data = dat, default = "EBICglasso"), args))
    mat <- as.matrix(fit$graph)
    provenance <- quicknet_backend_provenance("bootnet", "bootnet_EBICglasso", args, fit$arguments)
  } else if (model %in% c("correlation", "partial") || (model == "ordinal" && ordinal_method != "polychoric")) {
    args <- quicknet_backend_args(backend_args, stats::cor, reserved = c("x", "y", "use", "method"))
    cor_method <- if (model == "ordinal") ordinal_method else cor_method %||% "pearson"
    use <- if (repair_pd) "pairwise.complete.obs" else switch(missing, listwise = "complete.obs", pairwise = "pairwise.complete.obs", "everything")
    args <- c(args, list(use = use, method = cor_method))
    correlation_matrix <- do.call(stats::cor, c(list(x = dat), args))
    if (repair_pd) correlation_matrix <- quicknet_make_positive_definite(correlation_matrix)
    if (any(!is.finite(correlation_matrix))) stop("Correlation matrix contains non-finite values; select an explicit missing-data rule or check constant variables.", call. = FALSE)
    fit <- list(correlation = correlation_matrix)
    mat <- if (model == "partial") quicknet_partial_cor(correlation_matrix) else correlation_matrix
    gamma <- quicknet_resolve_gamma(model, gamma)
    provenance <- quicknet_backend_provenance("stats", "cor", args)
  } else if (model == "ising") {
    is_binary <- vapply(dat, function(x) all(stats::na.omit(unique(x)) %in% c(0, 1)), logical(1))
    if (!all(is_binary)) stop("Ising model requires all variables to be coded 0/1.", call. = FALSE)
    if (anyNA(dat)) stop("IsingFit does not support missing data; select missing = 'listwise' explicitly.", call. = FALSE)
    has_variation <- vapply(dat, function(x) length(unique(x)) == 2, logical(1))
    if (!all(has_variation)) stop("Ising model requires every variable to contain both 0 and 1. No variation in: ", paste(names(has_variation)[!has_variation], collapse = ", "), call. = FALSE)
    args <- quicknet_backend_args(backend_args, IsingFit::IsingFit,
      reserved = c("x", "family", "AND", "gamma"))
    gamma <- quicknet_resolve_gamma(model, gamma)
    args <- quicknet_merge_args(list(family = "binomial", AND = AND, gamma = gamma,
                                    plot = FALSE, progressbar = FALSE), args)
    fit <- do.call(IsingFit::IsingFit, c(list(x = dat), args))
    mat <- as.matrix(fit$weiadj)
    provenance <- quicknet_backend_provenance("IsingFit", "IsingFit", args)
  } else if (model == "ordinal") {
    args <- quicknet_backend_args(backend_args, psych::polychoric, reserved = c("x"))
    args <- quicknet_merge_args(list(progress = FALSE), args)
    fit <- do.call(psych::polychoric, c(list(x = dat), args))
    mat <- if (repair_pd) quicknet_make_positive_definite(fit$rho) else as.matrix(fit$rho)
    gamma <- quicknet_resolve_gamma(model, gamma)
    provenance <- quicknet_backend_provenance("psych", "polychoric", args)
  } else if (model == "mgm") {
    if (is.null(types) || is.null(levels)) stop("types and levels must be specified for mgm; the source estimator does not infer them.", call. = FALSE)
    quicknet_dynamic_validate(dat, node_names, types, levels)
    args <- quicknet_backend_args(backend_args, mgm::mgm, reserved = c("data", "type", "level"))
    if (!is.null(gamma) && !is.null(args$lambdaGam) && !identical(as.numeric(gamma), as.numeric(args$lambdaGam))) stop("gamma and lambdaGam conflict.", call. = FALSE)
    selection <- args$lambdaSel %||% "CV"
    gamma <- quicknet_resolve_gamma(model, gamma %||% args$lambdaGam, selection)
    args <- quicknet_merge_args(list(pbar = FALSE), args)
    if (!is.null(gamma)) args$lambdaGam <- gamma
    fit <- do.call(mgm::mgm, c(list(data = as.matrix(dat), type = types, level = levels), args))
    mat <- quicknet_apply_signs(fit$pairwise$wadj, fit$pairwise$signs)
    selection <- fit$call$lambdaSel
    gamma <- if (isFALSE(fit$call$regularize)) NULL else quicknet_resolve_gamma(model, fit$call$lambdaGam, selection)
    provenance <- quicknet_backend_provenance("mgm", "mgm", args, fit$call)
  }

  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- node_names
  node_table <- quicknet_node_table(mat)
  if (model %in% c("EBICglasso", "correlation", "partial", "ordinal")) {
    pred <- quicknet_continuous_predictability(dat)
    node_table <- merge(node_table, pred, by = "node", all.x = TRUE, sort = FALSE)
  }
  if (model == "ising" && !is.null(fit$thresholds)) {
    pred <- quicknet_binary_predictability(dat)
    node_table <- merge(node_table, pred, by = "node", all.x = TRUE, sort = FALSE)
    node_table$threshold <- as.numeric(fit$thresholds[node_table$node])
  }
  if (model == "mgm" && !is.null(types) && !is.null(levels)) {
    node_table$type <- types
    node_table$level <- levels
  }

  quicknet_fit(
    model = model,
    data = dat,
    networks = list(default = mat),
    nodes = node_table,
    fit = fit,
    meta = c(list(
      data_type = "cross_sectional",
      directed = FALSE,
      repair_pd = if (model %in% c("correlation", "partial", "ordinal")) repair_pd else NULL,
      missing = missing,
      cor_method = if (model %in% c("EBICglasso", "correlation", "partial", "ordinal")) cor_method else NULL,
      ordinal_method = if (model == "ordinal") ordinal_method else NULL,
      gamma = gamma,
      lambdaSel = selection,
      AND = if (model == "ising") AND else NULL,
      types = if (model == "mgm") types else NULL,
      levels = if (model == "mgm") levels else NULL,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    ), provenance)
  )
}

quicknet_refit_like <- function(data, fit) {
  args <- quicknet_cross_refit_args(fit)
  quicknet_check_row_args(args$backend_args, "Resampling")
  do.call(quicknet_fit_cross_sectional, c(list(data = data), args))
}

quicknet_cross_refit_args <- function(fit) {
  backend_args <- fit$meta$backend_args %||% list()
  raw <- if (is.list(fit$fit)) fit$fit else list()
  cor_method <- fit$meta$cor_method
  AND <- TRUE
  if (fit$model == "ising") {
    AND <- raw$AND %||% fit$meta$AND
    if (!is.logical(AND) || length(AND) != 1L || is.na(AND)) {
      stop("The fitted object's original Ising AND rule is unknown; refit the original data first.", call. = FALSE)
    }
  }
  if (fit$model == "mgm") {
    saved <- if (is.list(raw$call)) raw$call else list()
    saved <- saved[intersect(names(saved), setdiff(names(formals(mgm::mgm)), c("...", "data", "type", "level")))]
    if (!is.null(saved$weights) && all(saved$weights == 1)) saved$weights <- NULL
    backend_args <- quicknet_merge_args(backend_args, saved)
    backend_args$lambdaSel <- backend_args$lambdaSel %||% fit$meta$lambdaSel
    if (is.null(backend_args$lambdaSel)) stop("The fitted MGM selection method is unknown; refit the original data first.", call. = FALSE)
  }
  missing <- fit$meta$missing
  if (fit$model == "EBICglasso") {
    saved <- quicknet_saved_ebic_args(fit)
    backend_args <- quicknet_merge_args(backend_args, saved)
    # Earlier quickNet versions recorded cor_method even though bootnet did not
    # receive it. The estimator's own arguments take precedence.
    method <- saved$corMethod
    if (is.null(method)) {
      stop("The fitted object's original EBIC correlation estimator is unknown; refit the original data first.", call. = FALSE)
    }
    backend_args$corMethod <- method[[1L]]
    cor_method <- if (identical(method[[1L]], "cor")) saved$corArgs$method %||% "pearson" else NULL
    if (identical(missing, "none") && is.null(fit$meta$backend_args)) {
      missing <- saved$missing
      if (is.null(missing)) stop("The fitted object's original EBIC missing-data rule is unknown; refit the original data first.", call. = FALSE)
    }
  }
  reserved <- switch(fit$model, EBICglasso = c("tuning", "missing"), ising = c("family", "AND", "gamma"), correlation = c("use", "method"), partial = c("use", "method"), character())
  if (fit$model == "ordinal" && !identical(fit$meta$ordinal_method, "polychoric")) reserved <- c("use", "method")
  backend_args[intersect(names(backend_args), reserved)] <- NULL
  list(
    model = fit$model,
    cor_method = cor_method,
    missing = missing,
    gamma = quicknet_refit_gamma(fit),
    ordinal_method = fit$meta$ordinal_method %||% "polychoric",
    AND = AND,
    types = fit$meta$types,
    levels = fit$meta$levels,
    backend_args = backend_args,
    repair_pd = fit$meta$repair_pd %||% (fit$model %in% c("correlation", "partial", "ordinal") &&
      is.null(fit$meta$backend_args) && !is.null(fit$meta$call))
  )
}

# A bootstrap draw is retained only when every required network layer is usable.
# The original error is retained: failed fits are not replaced by a new draw.
quicknet_resampling_graph <- function(graph, original, layer = "default") {
  if (!is.matrix(graph) || !identical(dim(graph), dim(original))) {
    stop("Invalid dimensions in network layer '", layer, "'.", call. = FALSE)
  }
  if (!is.numeric(graph) || any(!is.finite(graph))) {
    stop("Non-finite weights in network layer '", layer, "'.", call. = FALSE)
  }
  if (!is.null(rownames(original)) && !is.null(rownames(graph)) &&
      !is.null(colnames(original)) && !is.null(colnames(graph))) {
    if (!setequal(rownames(original), rownames(graph)) ||
        !setequal(colnames(original), colnames(graph))) {
      stop("Mismatched nodes in network layer '", layer, "'.", call. = FALSE)
    }
    graph <- graph[rownames(original), colnames(original), drop = FALSE]
  }
  graph
}

quicknet_resampling_diagnostics <- function(reasons, method, unit, context) {
  failed <- !is.na(reasons)
  details <- data.frame(replication = which(failed), reason = reasons[failed],
                        stringsAsFactors = FALSE)
  info <- list(method = method, unit = unit, requested = length(reasons),
    succeeded = sum(!failed), failed = sum(failed),
    conditional_on_success = TRUE, failure_details = details)
  if (all(failed)) stop("All ", context, " failed; no valid result can be reported. First cause: ",
                        reasons[[1]], call. = FALSE)
  if (any(failed)) warning(sum(failed), " of ", length(failed), " ", context,
    " failed; summaries condition on successful fits. Reasons are retained in attr(result, 'resampling')$failure_details.",
    call. = FALSE)
  info
}

quicknet_bootstrap_edge_stability <- function(fit,
                                              nboot = 1000,
                                              seed = NULL,
                                              threshold = 1e-10) {
  if (!inherits(fit, "quicknet_fit")) stop("fit must be a quicknet_fit object.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  original <- fit$graph
  edge_array <- array(NA_real_, c(nrow(original), ncol(original), nboot))
  reasons <- rep(NA_character_, nboot)
  for (boot_index in seq_len(nboot)) {
    sampled_rows <- sample.int(nrow(fit$data), nrow(fit$data), replace = TRUE)
    graph <- tryCatch({
      boot_fit <- quicknet_refit_like(fit$data[sampled_rows, , drop = FALSE], fit)
      failure_reason <- quicknet_fit_failure_reason(boot_fit)
      if (!is.null(failure_reason)) stop(failure_reason, call. = FALSE)
      quicknet_resampling_graph(boot_fit$graph, original)
    }, error = function(e) { reasons[[boot_index]] <<- conditionMessage(e); NULL })
    if (!is.null(graph)) edge_array[, , boot_index] <- graph
  }
  diagnostics <- quicknet_resampling_diagnostics(reasons, "percentile_bootstrap", "observation",
                                                 "edge bootstrap replications")
  quicknet_matrix_bootstrap_summary(original, edge_array, threshold = threshold,
    failed_bootstraps = diagnostics$failed, diagnostics = diagnostics)
}

quicknet_case_drop_centrality_stability <- function(fit,
                                                    nboot = 1000,
                                                    proportions = c(0.10, 0.25, 0.50),
                                                    seed = NULL,
                                                    statistics = c("strength", "expected_influence")) {
  if (!inherits(fit, "quicknet_fit")) stop("fit must be a quicknet_fit object.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  original_centrality <- quicknet_node_table(fit$graph)
  if (!length(statistics) || any(!statistics %in% names(original_centrality))) {
    stop("Unknown case-drop centrality statistic.", call. = FALSE)
  }
  rows <- diagnostics <- list()
  for (drop_proportion in proportions) {
    keep_n <- min(nrow(fit$data), max(3, floor(nrow(fit$data) * (1 - drop_proportion))))
    correlations <- matrix(NA_real_, nboot, length(statistics), dimnames = list(NULL, statistics))
    reasons <- rep(NA_character_, nboot)
    for (boot_index in seq_len(nboot)) {
      sampled_rows <- sample.int(nrow(fit$data), keep_n, replace = FALSE)
      boot_centrality <- tryCatch({
        boot_fit <- quicknet_refit_like(fit$data[sampled_rows, , drop = FALSE], fit)
      failure_reason <- quicknet_fit_failure_reason(boot_fit)
      if (!is.null(failure_reason)) stop(failure_reason, call. = FALSE)
        graph <- quicknet_resampling_graph(boot_fit$graph, fit$graph)
        quicknet_node_table(graph)
      }, error = function(e) { reasons[[boot_index]] <<- conditionMessage(e); NULL })
      if (is.null(boot_centrality)) next
      for (statistic in statistics) {
        original_values <- original_centrality[[statistic]]
        boot_values <- boot_centrality[[statistic]]
        if (!all(is.finite(c(original_values, boot_values))) ||
            !isTRUE(stats::sd(original_values) > 0) || !isTRUE(stats::sd(boot_values) > 0)) next
        correlations[boot_index, statistic] <- stats::cor(original_values, boot_values)
      }
    }
    info <- quicknet_resampling_diagnostics(reasons, "case_drop_centrality_correlation", "observation",
                                            paste0("case-drop replications at proportion ", drop_proportion))
    info$requested_proportion_dropped <- drop_proportion
    info$undefined_by_statistic <- info$succeeded - colSums(is.finite(correlations))
    info$summary_denominator <- "finite centrality correlations among successful fits"
    info$observations_retained <- keep_n
    info$actual_proportion_dropped <- 1 - keep_n / nrow(fit$data)
    diagnostics[[as.character(drop_proportion)]] <- info
    for (statistic in statistics) {
      values <- correlations[, statistic]
      rows[[length(rows) + 1]] <- data.frame(
        proportion_dropped = drop_proportion,
        actual_proportion_dropped = info$actual_proportion_dropped,
        observations_retained = keep_n, statistic = statistic,
        bootstrap_reps = nboot, failed_reps = info$failed, successful_reps = info$succeeded,
        valid_reps = sum(is.finite(values)), undefined_reps = info$succeeded - sum(is.finite(values)),
        median_correlation = quicknet_safe_quantile(values, 0.5),
        q05_correlation = quicknet_safe_quantile(values, 0.05),
        q95_correlation = quicknet_safe_quantile(values, 0.95), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "resampling") <- diagnostics
  out
}

quicknet_matrix_bootstrap_summary <- function(original_matrix,
                                              edge_array,
                                              directed = FALSE,
                                              threshold = 1e-10,
                                              failed_bootstraps = 0,
                                              diagnostics = NULL) {
  original <- as.matrix(original_matrix)
  diag(original) <- 0
  node_names <- rownames(original) %||% colnames(original) %||% paste0("V", seq_len(ncol(original)))
  if (directed) {
    edge_index <- which(row(original) != col(original), arr.ind = TRUE)
    out <- data.frame(from = node_names[edge_index[, "col"]], to = node_names[edge_index[, "row"]],
                      stringsAsFactors = FALSE)
  } else {
    edge_index <- which(upper.tri(original), arr.ind = TRUE)
    out <- data.frame(node_i = node_names[edge_index[, "row"]], node_j = node_names[edge_index[, "col"]],
                      stringsAsFactors = FALSE)
  }
  values <- matrix(NA_real_, nrow = dim(edge_array)[3], ncol = nrow(edge_index))
  for (boot_index in seq_len(dim(edge_array)[3])) values[boot_index, ] <- edge_array[, , boot_index][edge_index]
  original_values <- original[edge_index]
  out$original_weight <- original_values
  out$bootstrap_mean <- apply(values, 2, quicknet_safe_mean)
  out$bootstrap_sd <- apply(values, 2, quicknet_safe_sd)
  out$ci_lower <- apply(values, 2, quicknet_safe_quantile, probability = 0.025)
  out$ci_upper <- apply(values, 2, quicknet_safe_quantile, probability = 0.975)
  out$selection_rate <- apply(abs(values) > threshold, 2, quicknet_safe_mean)
  out$sign_stability <- vapply(seq_along(original_values), function(edge_id) {
    if (abs(original_values[edge_id]) <= threshold) return(NA_real_)
    quicknet_safe_mean(sign(values[, edge_id]) == sign(original_values[edge_id]))
  }, numeric(1))
  out$requested_bootstraps <- dim(edge_array)[3]
  out$valid_bootstraps <- colSums(is.finite(values))
  out$failed_bootstraps <- failed_bootstraps
  out$successful_bootstraps <- dim(edge_array)[3] - failed_bootstraps
  out$undefined_bootstraps <- out$successful_bootstraps - out$valid_bootstraps
  out <- out[order(-abs(out$original_weight), -out$selection_rate), , drop = FALSE]
  attr(out, "resampling") <- diagnostics
  out
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

quicknet_is_ising_model <- function(model) {
  model %in% c("ising", "confirmatory_ising")
}

quicknet_supports_continuous_perturbation <- function(model) {
  model %in% c("EBICglasso", "correlation", "partial", "ordinal")
}
