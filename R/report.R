#' Extract report-ready model information
#'
#' @param fit A \code{quicknet_fit} object.
#' @param digits Number of digits used in the plain-text summary.
#' @param threshold Absolute edge-weight threshold used to count nonzero edges.
#'
#' @return A \code{quicknet_report} object containing sample, estimation,
#' network, edge, node, and model-specific reporting tables.
#' @export
quicknet_report <- function(fit, digits = 3, threshold = 1e-10) {
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit object.", call. = FALSE)
  }

  report <- list(
    model = fit$model,
    sample = quicknet_report_sample(fit),
    estimation = quicknet_report_estimation(fit),
    networks = quicknet_report_networks(fit, threshold = threshold),
    edges = quicknet_report_edges(fit, threshold = threshold),
    nodes = fit$nodes,
    model_specific = quicknet_report_model_specific(fit),
    text = quicknet_report_text(fit, digits = digits, threshold = threshold)
  )
  class(report) <- "quicknet_report"
  report
}

#' @export
print.quicknet_report <- function(x, ...) {
  cat("<quicknet_report>\n")
  cat(x$text, sep = "\n")
  invisible(x)
}

quicknet_report_sample <- function(fit) {
  data_type <- fit$meta$data_type %||% NA_character_
  if (identical(data_type, "cross_sectional")) {
    return(data.frame(
      data_type = data_type,
      observations = nrow(fit$data),
      nodes = ncol(fit$graph),
      stringsAsFactors = FALSE
    ))
  }

  if (identical(data_type, "panel")) {
    id <- fit$meta$id
    return(data.frame(
      data_type = data_type,
      subjects = length(unique(fit$data[[id]])),
      rows = nrow(fit$data),
      nodes = length(fit$meta$nodes),
      waves = length(fit$meta$waves),
      transitions = if (!is.null(fit$fit$design$predictors)) nrow(fit$fit$design$predictors) else NA_integer_,
      stringsAsFactors = FALSE
    ))
  }

  if (identical(data_type, "intensive_longitudinal")) {
    id <- fit$meta$id
    day <- fit$meta$day
    return(data.frame(
      data_type = data_type,
      subjects = length(unique(fit$data[[id]])),
      observations = nrow(fit$data),
      variables = length(fit$meta$vars),
      days = length(unique(fit$data[[day]])),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(data_type = data_type, stringsAsFactors = FALSE)
}

quicknet_report_estimation <- function(fit) {
  backend <- switch(
    fit$model,
    EBICglasso = "bootnet::estimateNetwork(default = 'EBICglasso')",
    correlation = "stats::cor",
    partial = "inverse correlation / precision matrix",
    ising = "IsingFit::IsingFit",
    ordinal = if (identical(fit$meta$ordinal_method, "polychoric")) "psych::polychoric" else "stats::cor",
    mgm = "mgm::mgm",
    clpn = "glmnet::cv.glmnet",
    graphicalVAR = "graphicalVAR::mlGraphicalVAR",
    mlVAR = "mlVAR::mlVAR",
    NA_character_
  )

  rows <- list(
    data.frame(parameter = "model", value = fit$model, stringsAsFactors = FALSE),
    data.frame(parameter = "backend", value = backend, stringsAsFactors = FALSE),
    data.frame(parameter = "data_type", value = fit$meta$data_type %||% NA_character_, stringsAsFactors = FALSE)
  )

  report_keys <- c(
    "n", "p", "missing", "cor_method", "ordinal_method", "gamma", "AND",
    "alpha", "lambda_rule", "nfolds", "standardize", "scale",
    "centerWithin", "lags", "estimator", "temporal", "contemporaneous",
    "nCores"
  )
  for (key in report_keys) {
    value <- fit$meta[[key]]
    if (!is.null(value)) {
      rows[[length(rows) + 1]] <- data.frame(
        parameter = key,
        value = quicknet_report_collapse(value),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!is.null(fit$meta$types)) {
    rows[[length(rows) + 1]] <- data.frame(parameter = "types", value = quicknet_report_collapse(fit$meta$types), stringsAsFactors = FALSE)
  }
  if (!is.null(fit$meta$levels)) {
    rows[[length(rows) + 1]] <- data.frame(parameter = "levels", value = quicknet_report_collapse(fit$meta$levels), stringsAsFactors = FALSE)
  }
  if (!is.null(fit$meta$waves)) {
    rows[[length(rows) + 1]] <- data.frame(parameter = "waves", value = quicknet_report_collapse(fit$meta$waves), stringsAsFactors = FALSE)
  }

  do.call(rbind, rows)
}

quicknet_report_networks <- function(fit, threshold = 1e-10) {
  rows <- lapply(names(fit$networks), function(network_name) {
    directed <- quicknet_report_is_directed(fit, network_name)
    out <- quicknet_network_summary(
      fit$networks[[network_name]],
      threshold = threshold,
      directed = directed
    )
    out$network <- network_name
    out[, c("network", setdiff(names(out), "network")), drop = FALSE]
  })
  do.call(rbind, rows)
}

quicknet_report_edges <- function(fit, threshold = 1e-10) {
  edges <- fit$edges
  if (is.null(edges) || nrow(edges) == 0) {
    return(data.frame())
  }
  networks <- unique(edges$network)
  rows <- lapply(networks, function(network_name) {
    net_edges <- edges[edges$network == network_name, , drop = FALSE]
    nonzero <- abs(net_edges$weight) > threshold
    data.frame(
      network = network_name,
      possible_edges = nrow(net_edges),
      nonzero_edges = sum(nonzero, na.rm = TRUE),
      positive_edges = sum(net_edges$weight > threshold, na.rm = TRUE),
      negative_edges = sum(net_edges$weight < -threshold, na.rm = TRUE),
      density = mean(nonzero, na.rm = TRUE),
      mean_abs_weight = ifelse(any(nonzero, na.rm = TRUE), mean(abs(net_edges$weight[nonzero]), na.rm = TRUE), 0),
      max_abs_weight = ifelse(any(nonzero, na.rm = TRUE), max(abs(net_edges$weight[nonzero]), na.rm = TRUE), 0),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  if ("edge_type" %in% names(edges)) {
    type_rows <- lapply(split(edges, list(edges$network, edges$edge_type), drop = TRUE), function(type_edges) {
      nonzero <- abs(type_edges$weight) > threshold
      data.frame(
        network = type_edges$network[[1]],
        edge_type = type_edges$edge_type[[1]],
        possible_edges = nrow(type_edges),
        nonzero_edges = sum(nonzero, na.rm = TRUE),
        mean_abs_weight = ifelse(any(nonzero, na.rm = TRUE), mean(abs(type_edges$weight[nonzero]), na.rm = TRUE), 0),
        stringsAsFactors = FALSE
      )
    })
    attr(out, "by_edge_type") <- do.call(rbind, type_rows)
  }

  out
}

quicknet_report_model_specific <- function(fit) {
  if (fit$model %in% c("EBICglasso", "correlation", "partial", "ordinal")) {
    return(fit$nodes[, intersect(c("node", "predictability_R2"), names(fit$nodes)), drop = FALSE])
  }

  if (fit$model == "ising") {
    return(fit$nodes[, intersect(c("node", "prevalence", "accuracy", "null_accuracy", "accuracy_gain", "threshold"), names(fit$nodes)), drop = FALSE])
  }

  if (fit$model == "mgm") {
    return(fit$nodes[, intersect(c("node", "type", "level", "strength", "expected_influence"), names(fit$nodes)), drop = FALSE])
  }

  if (fit$model == "clpn") {
    columns <- intersect(c("node", "autoregressive", "in_strength", "out_strength", "cv_r_squared", "lambda", "nonzero_predictors"), names(fit$nodes))
    return(fit$nodes[, columns, drop = FALSE])
  }

  if (fit$model %in% c("graphicalVAR", "mlVAR")) {
    return(fit$nodes)
  }

  data.frame()
}

quicknet_report_text <- function(fit, digits = 3, threshold = 1e-10) {
  sample <- quicknet_report_sample(fit)
  networks <- quicknet_report_networks(fit, threshold = threshold)
  default_network <- networks[networks$network == "default", , drop = FALSE]
  if (nrow(default_network) == 0) default_network <- networks[1, , drop = FALSE]

  sample_text <- if ("observations" %in% names(sample)) {
    paste0("n = ", sample$observations[[1]], ", nodes = ", default_network$nodes[[1]])
  } else if ("subjects" %in% names(sample)) {
    paste0("subjects = ", sample$subjects[[1]], ", nodes = ", default_network$nodes[[1]])
  } else {
    paste0("nodes = ", default_network$nodes[[1]])
  }

  paste0(
    "Model: ", fit$model, ". ",
    "Sample: ", sample_text, ". ",
    "Default network: ", default_network$nonzero_edges[[1]], " / ",
    default_network$possible_edges[[1]], " nonzero edges, density = ",
    round(default_network$density[[1]], digits), ", mean absolute edge weight = ",
    round(default_network$mean_abs_weight[[1]], digits), "."
  )
}

quicknet_report_is_directed <- function(fit, network_name) {
  if (fit$model == "clpn") return(TRUE)
  if (fit$model %in% c("graphicalVAR", "mlVAR")) {
    return(network_name %in% c("default", "temporal"))
  }
  isTRUE(fit$meta$directed)
}

quicknet_report_collapse <- function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(as.character(x), collapse = ", ")
}
