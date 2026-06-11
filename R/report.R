#' Extract report-ready model information
#'
#' @param fit A \code{quicknet_fit}, \code{quicknet_perturbation}, or
#' \code{quicknet_power} object.
#' @param digits Number of digits used in the plain-text summary.
#' @param threshold Absolute edge-weight threshold used to count nonzero edges.
#'
#' @return A \code{quicknet_report} object. For \code{quicknet_fit} inputs it
#' contains model registry metadata, sample, estimation, fit-index, parameter,
#' modification-index, constraint, network, edge, node, and model-specific
#' tables.
#' For \code{quicknet_perturbation} inputs it contains perturbation settings,
#' metrics, rankings, and a short text summary. For \code{quicknet_power}
#' inputs it contains design settings, simulation summaries, and sample-size
#' recommendations.
#' @export
quicknet_report <- function(fit, digits = 3, threshold = 1e-10) {
  if (inherits(fit, "quicknet_perturbation")) {
    return(quicknet_report_perturbation(fit, digits = digits))
  }
  if (inherits(fit, "quicknet_power")) {
    return(quicknet_report_power(fit, digits = digits))
  }
  if (!inherits(fit, "quicknet_fit")) {
    stop("fit must be a quicknet_fit, quicknet_perturbation, or quicknet_power object.", call. = FALSE)
  }

  report <- list(
    model = fit$model,
    model_info = quicknet_model_info(fit$model),
    sample = quicknet_report_sample(fit),
    estimation = quicknet_report_estimation(fit),
    fit_indices = quicknet_report_fit_indices(fit),
    parameters = quicknet_report_parameters(fit),
    modification_indices = quicknet_report_modification_indices(fit),
    constraints = quicknet_report_constraints(fit),
    networks = quicknet_report_networks(fit, threshold = threshold),
    edges = quicknet_report_edges(fit, threshold = threshold),
    nodes = fit$nodes,
    model_specific = quicknet_report_model_specific(fit),
    text = quicknet_report_text(fit, digits = digits, threshold = threshold)
  )
  class(report) <- "quicknet_report"
  report
}

quicknet_report_power <- function(fit, digits = 3) {
  settings <- data.frame(
    parameter = names(fit$settings),
    value = vapply(fit$settings, quicknet_report_collapse, character(1)),
    stringsAsFactors = FALSE
  )
  summary <- fit$summary
  numeric_columns <- vapply(summary, is.numeric, logical(1))
  summary[numeric_columns] <- lapply(summary[numeric_columns], round, digits)
  recommendation <- fit$recommendation
  numeric_recommendation <- vapply(recommendation, is.numeric, logical(1))
  recommendation[numeric_recommendation] <- lapply(recommendation[numeric_recommendation], round, digits)
  report <- list(
    model = fit$model,
    method = fit$method,
    settings = settings,
    summary = summary,
    recommendation = recommendation,
    text = fit$report
  )
  class(report) <- "quicknet_report"
  report
}

quicknet_report_perturbation <- function(fit, digits = 3) {
  metrics <- fit$metrics
  if ("burden_reduction" %in% names(metrics)) {
    best <- metrics[order(-metrics$burden_reduction), , drop = FALSE][1, ]
    text <- paste0(
      "Perturbation: ", fit$method, " for ", fit$model, ". ",
      "Best target/configuration: ", best$target[[1]], ", burden reduction = ",
      round(best$burden_reduction[[1]], digits), ". Results are model-implied in silico simulations."
    )
  } else if ("activity_reduction" %in% names(metrics)) {
    best <- metrics[order(-metrics$activity_reduction), , drop = FALSE][1, ]
    text <- paste0(
      "Perturbation: ", fit$method, " for ", fit$model, ". ",
      "Best target/configuration: ", best$target[[1]], ", activity reduction = ",
      round(best$activity_reduction[[1]], digits), ". Results are model-implied in silico simulations."
    )
  } else if ("spillover_blocked" %in% names(metrics)) {
    best <- metrics[order(-metrics$spillover_blocked), , drop = FALSE][1, ]
    text <- paste0(
      "Perturbation: ", fit$method, " for ", fit$model, ". ",
      "Best blocked edge: ", best$blocked_edge[[1]], ", spillover blocked = ",
      round(best$spillover_blocked[[1]], digits), ". Results are model-implied in silico simulations."
    )
  } else {
    text <- fit$report
  }

  report <- list(
    model = fit$model,
    method = fit$method,
    settings = data.frame(
      parameter = names(fit$settings),
      value = vapply(fit$settings, quicknet_report_collapse, character(1)),
      stringsAsFactors = FALSE
    ),
    metrics = fit$metrics,
    rankings = fit$rankings,
    text = text
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

  if (identical(data_type, "latent")) {
    return(data.frame(
      data_type = data_type,
      observations = nrow(fit$data),
      manifest_variables = fit$meta$p %||% ncol(fit$data),
      latent_variables = ncol(fit$networks$latent),
      stringsAsFactors = FALSE
    ))
  }

  if (identical(data_type, "time_series")) {
    return(data.frame(
      data_type = data_type,
      observations = nrow(fit$data),
      variables = length(fit$meta$vars),
      stringsAsFactors = FALSE
    ))
  }

  if (identical(data_type, "meta")) {
    return(data.frame(
      data_type = data_type,
      studies = fit$meta$n_studies %||% NA_integer_,
      variables = length(fit$meta$vars),
      total_n = if (!is.null(fit$meta$nobs)) sum(fit$meta$nobs, na.rm = TRUE) else NA_real_,
      rows = if (!is.null(fit$data)) nrow(fit$data) else NA_integer_,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(data_type = data_type, stringsAsFactors = FALSE)
}

quicknet_report_estimation <- function(fit) {
  model_info <- quicknet_model_info(fit$model)
  registry_backend <- if (nrow(model_info) > 0) model_info$backend[[1]] else NULL
  backend <- fit$meta$backend %||% registry_backend %||% switch(
    fit$model,
    EBICglasso = "bootnet::estimateNetwork(default = 'EBICglasso')",
    correlation = "stats::cor",
    partial = "inverse correlation / precision matrix",
    ising = "IsingFit::IsingFit",
    ordinal = if (identical(fit$meta$ordinal_method, "polychoric")) "psych::polychoric" else "stats::cor",
    mgm = "mgm::mgm",
    clpn = "glmnet::cv.glmnet",
    ri_clpm = "psychonetrics::ri_clpm",
    panel_gvar = "psychonetrics::panelgvar",
    panel_var = "psychonetrics::panelvar",
    panel_sem = "lavaan::sem",
    confirmatory_ggm = "psychonetrics::ggm",
    confirmatory_ising = "psychonetrics::Ising",
    confirmatory_cor = "psychonetrics::varcov(type = 'cor')",
    confirmatory_covariance = "psychonetrics::varcov(type = 'cov')",
    confirmatory_precision = "psychonetrics::varcov(type = 'prec')",
    latent_network = "lavaan::cfa",
    lvm = "psychonetrics::lvm",
    lnm = "psychonetrics::lnm",
    rnm = "psychonetrics::rnm",
    lrnm = "psychonetrics::lrnm",
    meta_ggm = "psychonetrics::meta_varcov(type = 'ggm')",
    meta_cor = "psychonetrics::meta_varcov(type = 'cor')",
    meta_gvar = "psychonetrics::meta_gvar",
    mixedVAR = "mgm::mvar",
    time_varying_mvar = "mgm::tvmvar",
    graphicalVAR = "graphicalVAR::mlGraphicalVAR",
    mlVAR = "mlVAR::mlVAR",
    psychonetrics_gvar = "psychonetrics::gvar",
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
    "nCores", "residual_cov", "lambdaSel", "bandwidth",
    "std.lv", "signInfo", "ri_type", "stationary", "identification",
    "randomEffects", "studyvar", "beta_model", "maxNodes"
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

quicknet_report_fit_indices <- function(fit) {
  fit_indices <- fit$fit$fit_indices
  if (!is.null(fit_indices)) {
    return(as.data.frame(fit_indices))
  }
  backend_model <- quicknet_report_backend_model(fit)
  if (quicknet_report_is_psychonetrics_model(backend_model)) {
    return(quicknet_psychonetrics_fit_indices(backend_model))
  }
  data.frame()
}

quicknet_report_parameters <- function(fit) {
  backend_model <- quicknet_report_backend_model(fit)
  if (quicknet_report_is_psychonetrics_model(backend_model)) {
    return(quicknet_report_psychonetrics_parameters(backend_model))
  }
  data.frame()
}

quicknet_report_modification_indices <- function(fit) {
  backend_model <- quicknet_report_backend_model(fit)
  if (!quicknet_report_is_psychonetrics_model(backend_model)) {
    return(data.frame())
  }
  out <- data.frame()
  invisible(utils::capture.output({
    out <- tryCatch(as.data.frame(psychonetrics::MIs(backend_model)), error = function(e) data.frame())
  }))
  if (!nrow(out)) return(out)
  signal_columns <- intersect(c("mi", "pmi", "epc", "mi_free", "pmi_free", "epc_free"), names(out))
  if (!length(signal_columns)) return(out[0, , drop = FALSE])
  has_signal <- Reduce(`|`, lapply(signal_columns, function(column) is.finite(suppressWarnings(as.numeric(out[[column]])))))
  out <- out[has_signal, , drop = FALSE]
  if ("mi" %in% names(out)) {
    mi <- suppressWarnings(as.numeric(out$mi))
    out <- out[order(-mi, na.last = TRUE), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

quicknet_report_constraints <- function(fit) {
  parameters <- quicknet_report_parameters(fit)
  if (!nrow(parameters) || !"matrix" %in% names(parameters)) {
    return(data.frame())
  }
  if (!"fixed" %in% names(parameters)) {
    parameters$fixed <- NA
  }
  if (!"group" %in% names(parameters)) {
    parameters$group <- NA_character_
  }
  groups <- split(parameters, list(parameters$matrix, parameters$group), drop = TRUE)
  rows <- lapply(groups, function(part) {
    fixed <- quicknet_report_as_logical(part$fixed)
    lower <- if ("minimum" %in% names(part)) suppressWarnings(as.numeric(part$minimum)) else rep(NA_real_, nrow(part))
    upper <- if ("maximum" %in% names(part)) suppressWarnings(as.numeric(part$maximum)) else rep(NA_real_, nrow(part))
    data.frame(
      matrix = part$matrix[[1]],
      group = part$group[[1]],
      parameters = nrow(part),
      free_parameters = sum(!fixed, na.rm = TRUE),
      fixed_parameters = sum(fixed, na.rm = TRUE),
      bounded_parameters = sum(is.finite(lower) | is.finite(upper), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

quicknet_report_networks <- function(fit, threshold = 1e-10) {
  quicknet_network_summary_list(fit$networks, model = fit$model, meta = fit$meta, threshold = threshold)
}

quicknet_report_edges <- function(fit, threshold = 1e-10) {
  edges <- fit$edges
  if (is.null(edges) || nrow(edges) == 0) {
    return(data.frame())
  }
  networks <- unique(edges$network)
  rows <- lapply(networks, function(network_name) {
    net_edges <- edges[edges$network == network_name, , drop = FALSE]
    self_edge <- net_edges$from == net_edges$to
    report_edges <- net_edges[!self_edge, , drop = FALSE]
    nonzero <- abs(report_edges$weight) > threshold
    data.frame(
      network = network_name,
      possible_edges = nrow(report_edges),
      self_edges = sum(self_edge, na.rm = TRUE),
      nonzero_self_edges = sum(abs(net_edges$weight[self_edge]) > threshold, na.rm = TRUE),
      total_edges_in_table = nrow(net_edges),
      nonzero_edges = sum(nonzero, na.rm = TRUE),
      positive_edges = sum(report_edges$weight > threshold, na.rm = TRUE),
      negative_edges = sum(report_edges$weight < -threshold, na.rm = TRUE),
      density = mean(nonzero, na.rm = TRUE),
      mean_abs_weight = ifelse(any(nonzero, na.rm = TRUE), mean(abs(report_edges$weight[nonzero]), na.rm = TRUE), 0),
      max_abs_weight = ifelse(any(nonzero, na.rm = TRUE), max(abs(report_edges$weight[nonzero]), na.rm = TRUE), 0),
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

  if (fit$model %in% c("graphicalVAR", "mlVAR", "psychonetrics_gvar", "confirmatory_ggm", "confirmatory_ising", "confirmatory_cor", "confirmatory_covariance", "confirmatory_precision", "latent_network", "lvm", "lnm", "rnm", "lrnm", "meta_ggm", "meta_cor", "meta_gvar", "mixedVAR", "time_varying_mvar", "panel_sem", "ri_clpm", "panel_gvar", "panel_var")) {
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
  quicknet_network_summary_is_directed(fit$model, fit$meta, network_name)
}

quicknet_report_collapse <- function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(as.character(x), collapse = ", ")
}

quicknet_report_backend_model <- function(fit) {
  if (is.null(fit$fit) || is.null(fit$fit$model)) return(NULL)
  fit$fit$model
}

quicknet_report_is_psychonetrics_model <- function(model) {
  inherits(model, "psychonetrics")
}

quicknet_report_psychonetrics_parameters <- function(model) {
  out <- data.frame()
  invisible(utils::capture.output({
    out <- tryCatch(as.data.frame(psychonetrics::parameters(model)), error = function(e) data.frame())
  }))
  if (!nrow(out)) return(out)
  out <- quicknet_report_add_parameter_ci(out)
  rownames(out) <- NULL
  out
}

quicknet_report_add_parameter_ci <- function(parameters) {
  if (!all(c("est", "se") %in% names(parameters))) {
    return(parameters)
  }
  est <- suppressWarnings(as.numeric(parameters$est))
  se <- suppressWarnings(as.numeric(parameters$se))
  valid <- is.finite(est) & is.finite(se)
  parameters$ci_lower <- NA_real_
  parameters$ci_upper <- NA_real_
  parameters$ci_lower[valid] <- est[valid] - stats::qnorm(0.975) * se[valid]
  parameters$ci_upper[valid] <- est[valid] + stats::qnorm(0.975) * se[valid]
  parameters
}

quicknet_report_as_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes")
}
