#' Create a quickNet fit object
#'
#' @param model Model name.
#' @param data Original input data.
#' @param networks Named list of network weight matrices.
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
  if (!is.list(networks) || is.null(names(networks)) || any(names(networks) == "")) {
    stop("networks must be a named list of matrices.", call. = FALSE)
  }

  networks <- lapply(networks, function(network) {
    network <- as.matrix(network)
    storage.mode(network) <- "double"
    network
  })

  default_network <- networks[[1]]
  edge_table <- if (is.null(edges)) quicknet_edge_table(default_network) else edges
  node_table <- if (is.null(nodes)) quicknet_node_table(default_network) else nodes
  summary_table <- if (is.null(network_summary)) {
    quicknet_network_summary_list(networks, model = model, meta = meta)
  } else {
    network_summary
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
      network_summary = summary_table,
      graph = default_network,
      graphData = list(graph = default_network),
      Edgelist = quicknet_edgelist(default_network, directed = isTRUE(meta$directed))
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
  invisible(x)
}

#' @export
summary.quicknet_fit <- function(object, ...) {
  object$network_summary
}

#' @export
plot.quicknet_fit <- function(x, network = "default", ...) {
  mat <- quicknet_network_matrix(x, network = network)
  qgraph::qgraph(mat, ...)
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
    density = mean(nonzero, na.rm = TRUE),
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
  if (model %in% c("clpn", "panel_sem", "mixedVAR", "time_varying_mvar")) return(TRUE)
  if (model %in% c("graphicalVAR", "mlVAR", "psychonetrics_gvar", "ri_clpm", "panel_gvar", "panel_var", "meta_gvar")) {
    return(network_name %in% c("default", "temporal", "cross_lagged"))
  }
  isTRUE(meta$directed)
}

quicknet_network_matrix <- function(x, network = "default") {
  if (inherits(x, "quicknet_fit")) {
    if (!network %in% names(x$networks)) {
      stop("Unknown network: ", network, call. = FALSE)
    }
    return(x$networks[[network]])
  }

  if (inherits(x, "qgraph")) {
    if (!is.null(x$graphData$graph)) return(as.matrix(x$graphData$graph))
    if (!is.null(x$graph)) return(as.matrix(x$graph))
  }

  if (is.matrix(x) || is.data.frame(x)) {
    return(as.matrix(x))
  }

  if (is.list(x) && !is.null(x$graph)) {
    return(as.matrix(x$graph))
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
    network = network,
    from = node_names[index[, "col"]],
    to = node_names[index[, "row"]],
    from_index = index[, "col"],
    to_index = index[, "row"],
    weight = mat[index],
    directed = directed,
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
    fit <- stats::lm(stats::as.formula(paste(target, "~", paste(predictors, collapse = " + "))), data = model_data)
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
      stats::glm(stats::as.formula(paste(target, "~", paste(predictors, collapse = " + "))), data = model_data, family = stats::binomial()),
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
  correlation_matrix <- quicknet_make_positive_definite(correlation_matrix)
  precision <- solve(correlation_matrix)
  partial <- -stats::cov2cor(precision)
  diag(partial) <- 0
  colnames(partial) <- rownames(partial) <- colnames(correlation_matrix)
  partial
}

quicknet_fit_cross_sectional <- function(data,
                                         model = c("EBICglasso", "correlation", "partial", "ising", "ordinal", "mgm"),
                                         cor_method = c("pearson", "spearman", "kendall"),
                                         missing = c("listwise", "none"),
                                         gamma = 0.5,
                                         ordinal_method = c("polychoric", "spearman", "pearson"),
                                         AND = TRUE,
                                         types = NULL,
                                         levels = NULL) {
  model <- match.arg(model)
  cor_method <- match.arg(cor_method)
  missing <- match.arg(missing)
  ordinal_method <- match.arg(ordinal_method)

  dat <- quicknet_complete_numeric_data(data, missing = missing)
  node_names <- colnames(dat)

  if (model == "EBICglasso") {
    fit <- bootnet::estimateNetwork(dat, default = "EBICglasso", tuning = gamma, verbose = FALSE)
    mat <- as.matrix(fit$graph)
  } else if (model %in% c("correlation", "partial")) {
    correlation_matrix <- stats::cor(dat, use = "pairwise.complete.obs", method = cor_method)
    correlation_matrix <- quicknet_make_positive_definite(correlation_matrix)
    fit <- list(correlation = correlation_matrix)
    mat <- if (model == "correlation") correlation_matrix else quicknet_partial_cor(correlation_matrix)
  } else if (model == "ising") {
    dat[] <- lapply(dat, as.integer)
    is_binary <- vapply(dat, function(x) all(stats::na.omit(unique(x)) %in% c(0L, 1L)), logical(1))
    if (!all(is_binary)) {
      stop("Ising model requires all variables to be coded 0/1.", call. = FALSE)
    }
    has_variation <- vapply(dat, function(x) length(unique(x)) == 2, logical(1))
    if (!all(has_variation)) {
      stop(
        "Ising model requires every variable to contain both 0 and 1. No variation in: ",
        paste(names(has_variation)[!has_variation], collapse = ", "),
        call. = FALSE
      )
    }

    fit <- IsingFit::IsingFit(
      x = dat,
      family = "binomial",
      AND = AND,
      gamma = gamma,
      plot = FALSE,
      progressbar = FALSE
    )
    mat <- as.matrix(fit$weiadj)
  } else if (model == "ordinal") {
    dat[] <- lapply(dat, as.integer)
    if (ordinal_method == "polychoric") {
      fit <- psych::polychoric(
        dat,
        smooth = TRUE,
        correct = 0.5,
        progress = FALSE,
        na.rm = TRUE,
        max.cat = max(vapply(dat, function(x) length(unique(x)), integer(1)))
      )
      mat <- fit$rho
    } else {
      mat <- stats::cor(dat, use = "pairwise.complete.obs", method = ordinal_method)
      fit <- list(correlation = mat, method = ordinal_method)
    }
    mat <- quicknet_make_positive_definite(mat)
  } else if (model == "mgm") {
    if (is.null(types)) {
      types <- rep("g", ncol(dat))
    }
    if (is.null(levels)) {
      levels <- ifelse(types == "g", 1L, vapply(dat, function(x) length(unique(x)), integer(1)))
    }
    if (length(types) != ncol(dat) || length(levels) != ncol(dat)) {
      stop("types and levels must have one entry per network variable.", call. = FALSE)
    }
    fit <- mgm::mgm(
      data = as.matrix(dat),
      type = types,
      level = levels,
      k = 2,
      lambdaSel = "EBIC",
      lambdaGam = gamma,
      ruleReg = "OR",
      scale = TRUE,
      pbar = FALSE,
      signInfo = FALSE,
      warnings = FALSE
    )
    mat <- as.matrix(fit$pairwise$wadj)
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
    meta = list(
      data_type = "cross_sectional",
      directed = FALSE,
      missing = missing,
      cor_method = cor_method,
      ordinal_method = if (model == "ordinal") ordinal_method else NULL,
      gamma = gamma,
      AND = if (model == "ising") AND else NULL,
      types = if (model == "mgm") types else NULL,
      levels = if (model == "mgm") levels else NULL,
      n = nrow(dat),
      p = ncol(dat),
      call = match.call()
    )
  )
}

quicknet_refit_like <- function(data, fit) {
  quicknet_fit_cross_sectional(
    data = data,
    model = fit$model,
    cor_method = fit$meta$cor_method %||% "pearson",
    missing = fit$meta$missing %||% "listwise",
    gamma = fit$meta$gamma %||% 0.5,
    ordinal_method = fit$meta$ordinal_method %||% "polychoric",
    AND = fit$meta$AND %||% TRUE,
    types = fit$meta$types,
    levels = fit$meta$levels
  )
}

quicknet_bootstrap_edge_stability <- function(fit,
                                              nboot = 1000,
                                              seed = NULL,
                                              threshold = 1e-10) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  data <- fit$data
  original <- fit$graph
  node_names <- colnames(original)
  edge_index <- which(upper.tri(original), arr.ind = TRUE)
  edge_values <- matrix(NA_real_, nrow = nboot, ncol = nrow(edge_index))
  failed <- logical(nboot)

  for (boot_index in seq_len(nboot)) {
    sampled_rows <- sample(seq_len(nrow(data)), nrow(data), replace = TRUE)
    boot_fit <- tryCatch(
      quicknet_refit_like(data[sampled_rows, , drop = FALSE], fit),
      error = function(e) NULL
    )
    if (is.null(boot_fit) || !all(dim(boot_fit$graph) == dim(original))) {
      failed[boot_index] <- TRUE
      next
    }
    edge_values[boot_index, ] <- boot_fit$graph[edge_index]
  }

  original_values <- original[edge_index]
  out <- data.frame(
    node_i = node_names[edge_index[, "row"]],
    node_j = node_names[edge_index[, "col"]],
    original_weight = original_values,
    bootstrap_mean = colMeans(edge_values, na.rm = TRUE),
    bootstrap_sd = apply(edge_values, 2, stats::sd, na.rm = TRUE),
    ci_lower = apply(edge_values, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    ci_upper = apply(edge_values, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
    selection_rate = colMeans(abs(edge_values) > threshold, na.rm = TRUE),
    valid_bootstraps = colSums(is.finite(edge_values)),
    failed_bootstraps = sum(failed),
    stringsAsFactors = FALSE
  )
  out$sign_stability <- vapply(seq_along(original_values), function(edge_id) {
    if (abs(original_values[edge_id]) <= threshold) return(NA_real_)
    mean(sign(edge_values[, edge_id]) == sign(original_values[edge_id]), na.rm = TRUE)
  }, numeric(1))
  out[order(-abs(out$original_weight), -out$selection_rate), , drop = FALSE]
}

quicknet_case_drop_centrality_stability <- function(fit,
                                                    nboot = 1000,
                                                    proportions = c(0.10, 0.25, 0.50),
                                                    seed = NULL,
                                                    statistics = c("strength", "expected_influence")) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  data <- fit$data
  original_centrality <- quicknet_node_table(fit$graph)
  rows <- list()

  for (drop_proportion in proportions) {
    keep_n <- max(3, floor(nrow(data) * (1 - drop_proportion)))
    correlations <- matrix(NA_real_, nrow = nboot, ncol = length(statistics), dimnames = list(NULL, statistics))

    for (boot_index in seq_len(nboot)) {
      sampled_rows <- sample(seq_len(nrow(data)), keep_n, replace = FALSE)
      boot_fit <- tryCatch(
        quicknet_refit_like(data[sampled_rows, , drop = FALSE], fit),
        error = function(e) NULL
      )
      if (is.null(boot_fit) || !all(dim(boot_fit$graph) == dim(fit$graph))) next

      boot_centrality <- quicknet_node_table(boot_fit$graph)
      for (statistic in statistics) {
        original_values <- original_centrality[[statistic]]
        boot_values <- boot_centrality[[statistic]]
        if (stats::sd(original_values, na.rm = TRUE) == 0 || stats::sd(boot_values, na.rm = TRUE) == 0) next
        correlations[boot_index, statistic] <- stats::cor(original_values, boot_values, use = "complete.obs")
      }
    }

    for (statistic in statistics) {
      values <- correlations[, statistic]
      finite_values <- values[is.finite(values)]
      rows[[length(rows) + 1]] <- data.frame(
        proportion_dropped = drop_proportion,
        statistic = statistic,
        bootstrap_reps = nboot,
        valid_reps = length(finite_values),
        median_correlation = ifelse(length(finite_values) > 0, stats::median(finite_values), NA_real_),
        q05_correlation = ifelse(length(finite_values) > 0, stats::quantile(finite_values, 0.05), NA_real_),
        q95_correlation = ifelse(length(finite_values) > 0, stats::quantile(finite_values, 0.95), NA_real_),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

quicknet_matrix_bootstrap_summary <- function(original_matrix,
                                              edge_array,
                                              directed = FALSE,
                                              threshold = 1e-10,
                                              failed_bootstraps = 0) {
  original <- as.matrix(original_matrix)
  diag(original) <- 0
  node_names <- rownames(original)
  if (is.null(node_names)) node_names <- colnames(original)
  if (is.null(node_names)) node_names <- paste0("V", seq_len(ncol(original)))

  if (directed) {
    edge_index <- which(row(original) != col(original), arr.ind = TRUE)
    out <- data.frame(
      from = node_names[edge_index[, "col"]],
      to = node_names[edge_index[, "row"]],
      stringsAsFactors = FALSE
    )
  } else {
    edge_index <- which(upper.tri(original), arr.ind = TRUE)
    out <- data.frame(
      node_i = node_names[edge_index[, "row"]],
      node_j = node_names[edge_index[, "col"]],
      stringsAsFactors = FALSE
    )
  }

  values <- matrix(NA_real_, nrow = dim(edge_array)[3], ncol = nrow(edge_index))
  for (boot_index in seq_len(dim(edge_array)[3])) {
    mat <- edge_array[, , boot_index]
    diag(mat) <- 0
    values[boot_index, ] <- mat[edge_index]
  }

  original_values <- original[edge_index]
  out$original_weight <- original_values
  out$bootstrap_mean <- colMeans(values, na.rm = TRUE)
  out$bootstrap_sd <- apply(values, 2, stats::sd, na.rm = TRUE)
  out$ci_lower <- apply(values, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
  out$ci_upper <- apply(values, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
  out$selection_rate <- colMeans(abs(values) > threshold, na.rm = TRUE)
  out$sign_stability <- vapply(seq_along(original_values), function(edge_id) {
    if (abs(original_values[edge_id]) <= threshold) return(NA_real_)
    mean(sign(values[, edge_id]) == sign(original_values[edge_id]), na.rm = TRUE)
  }, numeric(1))
  out$valid_bootstraps <- colSums(is.finite(values))
  out$failed_bootstraps <- failed_bootstraps
  out[order(-abs(out$original_weight), -out$selection_rate), , drop = FALSE]
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
