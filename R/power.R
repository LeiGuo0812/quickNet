#' Network recovery simulation and sample size planning
#'
#' @param method "netSimulator" (default) directly wraps bootnet::netSimulator
#'   for recovery at prespecified sample sizes. "powerly" wraps powerly::powerly
#'   for automated GGM sample-size search.
#' @param nodes Number of nodes for powerly's generator. Unnecessary when a
#'   powerly model_matrix is supplied.
#' @param density Nonzero edge density for the powerly generator.
#' @param positive Positive-edge proportion for those generators.
#' @param edge_strength Absolute edge-weight range for those generators.
#' @param sample_sizes Alias for netSimulator's nCases; NULL inherits its native
#'   grid. For powerly use range_lower and range_upper in ... .
#' @param replications Alias for native nReps for netSimulator. NULL inherits
#'   the backend default: 100 for netSimulator, 30 for powerly.
#' @param target_metric Powerly recovery criterion: sensitivity, specificity,
#'   mcc or edge_weight_correlation. Not applicable to descriptive netSimulator
#'   studies.
#' @param target_value Performance threshold, distinct from target_probability.
#' @param target_probability Required probability of attaining target_value.
#' @param gamma For netSimulator an explicitly supplied value aliases native
#'   tuning; use tuning in ... for multiple conditions. Powerly's
#'   public API cannot configure gamma; a non-NULL value is rejected.
#' @param seed Optional random seed. NULL uses the current RNG stream.
#' @param model_matrix Explicit hypothesized network. For netSimulator this
#'   is native input: a matrix, graph/intercepts list, or generator function.
#'   For powerly it is a symmetric partial-correlation matrix with zero diagonal.
#' @param model Native powerly model name. NULL or "ggm" is supported; other
#'   model names are not implemented by powerly's public API.
#' @param ... Native arguments. netSimulator accepts default, dataGenerator,
#'   nCases, nReps, nCores, estimation conditions such as tuning, moreArgs and
#'   moreOutput. Powerly accepts range_lower, range_upper, samples, measure,
#'   measure_value, statistic_value, boots, tolerance, iterations and cores.
#'   SampleSize forwards its arguments unchanged to NetworkPower.
#'
#' @details netSimulator implements the recovery study described by Epskamp and
#'   Fried (2018). It preserves native edge/centrality measures, conditions,
#'   errors and undefined values in fit and results. It does not automatically
#'   recommend N or apply a target-attainment rule. summary and plot dispatch
#'   to native methods. A matrix/list fixes the assumed network; a native input
#'   function may generate a different true network for each replication.
#'   If a reference network is estimated from data, the tutorial suggests an
#'   unregularized refit of its selected structure (refit = TRUE); quickNet never
#'   automatically refits, rescales or modifies a supplied network.
#'
#'   Powerly implements iterative Monte Carlo, monotone curve-fitting and
#'   stratified bootstrap (Constantin et al., 2023). Its native object, sample-size
#'   interval and convergence diagnostics are retained. The bootstrap-median
#'   curve determines its median N. Native GGM generation uses five ordinal
#'   levels and its estimator uses gamma = 0.5. The source replaces undefined
#'   recovery values by zero. Use ValidateNetworkPower for native independent
#'   validation. An interpolated candidate is not evidence of guaranteed recovery.
#' @return A quicknet_power object retaining the backend fit, recovery summary,
#'   assumptions and conditional recommendation (not applicable for netSimulator).
#' @references Epskamp, S., & Fried, E. I. (2018). A tutorial on regularized
#'   partial correlation networks. Psychological Methods, 23, 617--634.
#'   \doi{10.1037/met0000167}.
#'   Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2023).
#'   A general Monte Carlo method for sample size analysis in the context of
#'   network models. \doi{10.1037/met0000555}.
#' @export
NetworkPower <- function(method = c("netSimulator", "powerly"),
                         nodes = NULL,
                         density = NULL,
                         positive = NULL,
                         edge_strength = NULL,
                         sample_sizes = NULL,
                         replications = NULL,
                         target_metric = NULL,
                         target_value = 0.60,
                         target_probability = 0.80,
                         gamma = NULL,
                         seed = NULL,
                         model_matrix = NULL,
                         model = NULL,
                         ...) {
  supplied <- names(match.call())
  method <- match.arg(method)
  dots <- list(...)
  removed <- intersect(names(dots), c("estimator", "powerly_args"))
  if (length(removed)) stop("Removed sample-size controls: ", paste(removed, collapse = ", "),
    ". Use native backend arguments directly; netSimulator selects its estimator with default.", call. = FALSE)
  if (method == "netSimulator") {
    inactive <- intersect(supplied, c("nodes", "density", "positive", "edge_strength",
      "target_metric", "target_value", "target_probability", "model"))
    if (length(inactive)) stop("These controls do not apply to netSimulator: ",
      paste(inactive, collapse = ", "), ". Use an explicit model_matrix and native backend arguments.", call. = FALSE)
    if (!is.null(gamma)) {
      if ("tuning" %in% names(dots)) stop("Supply gamma or native tuning, not both.", call. = FALSE)
      dots$tuning <- quicknet_resolve_gamma("EBICglasso", gamma)
    }
    return(quicknet_power_netsimulator(model_matrix, sample_sizes, replications, seed, dots))
  }
  if (!is.null(gamma)) stop("powerly's public API cannot configure gamma; its native GGM estimator uses gamma = 0.5.", call. = FALSE)
  if (!is.null(model) && !identical(model, "ggm")) {
    stop("powerly's public API supports only model = 'ggm'.", call. = FALSE)
  }
  if (!is.null(sample_sizes)) stop("Use range_lower and range_upper for powerly; sample_sizes applies to netSimulator.", call. = FALSE)
  if (!is.null(model_matrix)) {
    quicknet_power_validate_true_matrix(model_matrix)
    dots$model_matrix <- model_matrix
    nodes <- nrow(model_matrix)
    density <- mean(model_matrix[upper.tri(model_matrix)] != 0)
  } else if (is.null(nodes) || is.null(density)) {
    stop("nodes and density must be specified for powerly's network generator, unless model_matrix is supplied.", call. = FALSE)
  }
  if (!quicknet_is_positive_integer(nodes) || nodes < 2L) {
    stop("nodes must be an integer of at least 2.", call. = FALSE)
  }
  target_metric <- match.arg(target_metric %||% "sensitivity",
    c("sensitivity", "specificity", "mcc", "edge_weight_correlation"))
  quicknet_power_validate_target(target_metric, target_value, target_probability)
  quicknet_power_powerly(
    nodes = nodes, density = density,
    positive = positive %||% 0.9, edge_strength = edge_strength %||% c(0.5, 1),
    target_metric = target_metric, target_value = target_value,
    target_probability = target_probability, seed = seed,
    backend_args = dots, replications = replications %||% 30
  )
}

#' @rdname NetworkPower
#' @export
SampleSize <- function(...) {
  NetworkPower(...)
}

#' @export
print.quicknet_power <- function(x, ...) {
  cat("<quicknet_power>\n")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Model: ", x$model, "\n", sep = "")
  if (identical(x$method, "netSimulator")) {
    cat(x$report, "\n", sep = "")
    return(invisible(x))
  }
  if (!is.null(x$recommendation$recommended_n) &&
      is.finite(x$recommendation$recommended_n[[1]])) {
    cat("Recommended N: ", x$recommendation$recommended_n, "\n", sep = "")
  } else {
    cat("Recommended N: not reached in evaluated range\n")
  }
  cat(x$report, "\n", sep = "")
  invisible(x)
}

#' @export
summary.quicknet_power <- function(object, ...) {
  if (identical(object$method, "netSimulator")) return(summary(object$fit, ...))
  object$summary
}

#' Plot native network recovery and sample size planning results
#'
#' @param x A quicknet_power object from a native network simulation or search.
#' @param ... Arguments forwarded to native plot methods: e.g. yvar for
#'   netSimulator and step for powerly.
#' @return The native backend plot result.
#' @export
plot.quicknet_power <- function(x, ...) {
  if (!inherits(x, "quicknet_power") || !x$method %in% c("netSimulator", "powerly")) {
    stop("x must be a native netSimulator or powerly quicknet_power object.", call. = FALSE)
  }
  plot(x$fit, ...)
}

quicknet_power_powerly <- function(nodes,
                                   density,
                                   positive,
                                   edge_strength,
                                   target_metric,
                                   target_value,
                                   target_probability,
                                   seed,
                                   backend_args,
                                   replications = 30) {
  if (!requireNamespace("powerly", quietly = TRUE)) {
    stop("Package 'powerly' is required for NetworkPower(method = 'powerly').", call. = FALSE)
  }
  metric_map <- c(
    sensitivity = "sen",
    specificity = "spe",
    mcc = "mcc",
    edge_weight_correlation = "rho"
  )
  if (!target_metric %in% names(metric_map)) {
    stop("powerly backend supports sensitivity, specificity, mcc, and edge_weight_correlation.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  defaults <- list(
    replications = replications,
    model = "ggm",
    nodes = nodes,
    density = density,
    positive = positive,
    range = edge_strength,
    measure = unname(metric_map[[target_metric]]),
    statistic = "power",
    measure_value = target_value,
    statistic_value = target_probability,
    monotone = TRUE,
    increasing = TRUE,
    lower_ci = 0.025,
    upper_ci = 0.975,
    verbose = FALSE
  )
  backend_args <- quicknet_backend_args(backend_args, powerly::powerly,
    reserved = c("model"), extra = c("nodes", "density", "positive", "constant", "range"))
  args <- quicknet_merge_args(defaults, backend_args)
  if (is.null(args$range_lower) || is.null(args$range_upper)) stop("range_lower and range_upper must be specified; powerly has no defaults for them.", call. = FALSE)
  if (!quicknet_is_positive_integer(args$range_lower) || !quicknet_is_positive_integer(args$range_upper) ||
      args$range_upper <= args$range_lower) stop("range_lower and range_upper must be positive integers with range_lower < range_upper.", call. = FALSE)
  if (!quicknet_is_positive_integer(args$replications)) stop("replications must be a positive integer.", call. = FALSE)
  if (args$statistic != "power") stop("NetworkPower requires statistic = 'power' to report target-achievement probabilities.", call. = FALSE)
  if (!args$measure %in% metric_map) stop("Unsupported powerly measure.", call. = FALSE)
  target_metric <- names(metric_map)[match(args$measure, metric_map)]
  target_value <- args$measure_value
  target_probability <- args$statistic_value
  quicknet_power_validate_target(target_metric, target_value, target_probability)
  if (!is.null(args$model_matrix)) {
    quicknet_power_validate_true_matrix(args$model_matrix)
    args[c("nodes", "density", "positive", "range", "constant")] <- NULL
  }
  for (name in c("samples", "boots", "tolerance", "iterations", "cores", "cluster_type", "save_memory", "solver_type", "spline_df")) {
    if (!name %in% names(args)) args[name] <- list(quicknet_backend_default(powerly::powerly, name))
  }
  fit <- do.call(powerly::powerly, args)
  recommendation <- quicknet_power_powerly_recommendation(fit, target_probability)
  summary <- quicknet_power_powerly_summary(fit, target_metric, target_value)
  true_network <- tryCatch(fit$step_1$true_model_parameters, error = function(e) NULL)
  native_estimator <- get("GgmModel", asNamespace("powerly"))$public_methods$estimate
  settings <- c(args, list(seed = seed, target_metric = target_metric,
    target_value = target_value, target_probability = target_probability,
    backend_version = as.character(utils::packageVersion("powerly")),
    backend_estimator = "qgraph::EBICglasso", backend_gamma = quicknet_backend_default(native_estimator, "gamma"),
    backend_data_levels = quicknet_backend_default(get("GgmModel", asNamespace("powerly"))$public_methods$generate, "levels"),
    estimand = "partial_correlation", conditioning = "one_fixed_generating_network_per_call",
    denominator_policy = "source_replaces_NA_measures_with_zero_before_computing_power",
    probability_comparison = recommendation$probability_comparison %||% ">="))
  # Native convergence concerns the bootstrap sample-size interval, not whether
  # a recovery target is attained. Keep both states independently visible.
  settings$algorithm_converged <- tryCatch(fit$converged, error = function(e) NULL)
  settings$algorithm_iterations <- tryCatch(fit$iteration, error = function(e) NULL)
  settings$algorithm_duration <- tryCatch(fit$duration, error = function(e) NULL)
  settings$native_algorithm <- "iterative_monte_carlo_monotone_spline_stratified_bootstrap"
  settings$reference <- "10.1037/met0000555"
  recommendation$algorithm_converged <- settings$algorithm_converged %||% NA
  recommendation$algorithm_iterations <- settings$algorithm_iterations %||% NA_integer_
  recommendation$recommendation_interval_width <-
    (recommendation$backend_n_upper %||% NA_real_) - (recommendation$backend_n_lower %||% NA_real_)
  if (!is.null(true_network)) {
    values <- true_network[upper.tri(true_network)]
    settings$generated_nodes <- nrow(true_network)
    settings$generated_density <- mean(values != 0)
    settings$generated_positive <- if (any(values != 0)) mean(values[values != 0] > 0) else NA_real_
    settings$generated_edge_strength <- if (any(values != 0)) range(abs(values[values != 0])) else c(NA_real_, NA_real_)
    settings$target_defined_for_truth <- quicknet_power_target_defined(true_network, target_metric, 0)
    if (!settings$target_defined_for_truth) {
      recommendation$recommended_n <- NA_real_
      recommendation$reached <- FALSE
    }
  }
  report <- if (isTRUE(recommendation$reached[[1]])) {
    paste0("Powerly bootstrap median sample-size estimate = ", recommendation$recommended_n[[1]],
           "; the bootstrap-median target-achievement curve at this estimate is ",
           signif(recommendation$achieved_probability[[1]], 3),
           " (target ", settings$probability_comparison, " ", target_probability,
           "). This is the source software's interpolated estimate; validate it with ValidateNetworkPower(plan) or powerly::validate(plan$fit).")
  } else if (identical(settings$target_defined_for_truth, FALSE)) {
    "The requested recovery metric is undefined for this true network. Powerly replaces undefined measures with zero; its numerical recommendation cannot establish target attainment."
  } else {
    "Powerly's bootstrap-median curve did not meet the requested probability at the returned sample size. Extend the evaluated range or increase simulation precision."
  }
  if (identical(settings$algorithm_converged, FALSE)) report <- paste(report,
    "The native search has not converged within the allowed iterations; this estimate is provisional.")
  quicknet_power_object(
    method = "powerly",
    model = "ggm",
    settings = settings,
    true_network = true_network,
    results = NULL,
    summary = summary,
    recommendation = recommendation,
    fit = fit,
    report = paste0(report, " Results are conditional on the source's true network and its data-generation settings (",
                    settings$backend_data_levels, " ordinal levels by default)."),
    generating_network = true_network
  )
}

quicknet_power_binomial <- function(successes, trials) {
  if (trials < 1) return(list(probability = NA_real_, mcse = NA_real_, lower = NA_real_, upper = NA_real_))
  probability <- successes / trials
  list(probability = probability, mcse = sqrt(probability * (1 - probability) / trials),
       lower = if (successes == 0) 0 else stats::qbeta(0.025, successes, trials - successes + 1),
       upper = if (successes == trials) 1 else stats::qbeta(0.975, successes + 1, trials - successes))
}

quicknet_power_powerly_recommendation <- function(fit, target_probability) {
  rec <- tryCatch(fit$recommendation, error = function(e) NULL)
  recommended_n <- if (!is.null(rec) && length(rec)) {
    if ("50%" %in% names(rec)) as.numeric(rec[["50%"]]) else as.numeric(rec[[1]])
  } else NA_real_
  curve_x <- tryCatch(as.numeric(fit$step_2$interpolation$x), error = function(e) numeric())
  point_y <- tryCatch(as.numeric(fit$step_2$interpolation$fitted), error = function(e) numeric())
  ci <- tryCatch(fit$step_3$ci, error = function(e) NULL)
  has_median <- is.matrix(ci) && "50%" %in% colnames(ci) && nrow(ci) == length(curve_x)
  curve_y <- if (has_median) ci[, "50%"] else rep(NA_real_, length(curve_x))
  curve_source <- if (has_median) "bootstrap_median_curve" else "bootstrap_median_unavailable"
  evaluate <- function(y) {
    if (length(curve_x) < 2 || length(curve_x) != length(y) || !is.finite(recommended_n)) return(NA_real_)
    valid <- is.finite(curve_x) & is.finite(y)
    if (sum(valid) < 2) return(NA_real_)
    stats::approx(curve_x[valid], y[valid], xout = recommended_n, rule = 1)$y
  }
  achieved <- evaluate(curve_y)
  monotone <- tryCatch(fit$step_2$spline$basis$monotone, error = function(e) NULL) %||% TRUE
  increasing <- tryCatch(fit$step_2$spline$solver$increasing, error = function(e) NULL) %||% TRUE
  decreasing <- isTRUE(monotone) && isFALSE(increasing)
  reached <- is.finite(achieved) && if (decreasing) achieved <= target_probability else achieved >= target_probability
  lower <- if (any(is.finite(curve_x))) min(curve_x[is.finite(curve_x)]) else NA_real_
  upper <- if (any(is.finite(curve_x))) max(curve_x[is.finite(curve_x)]) else NA_real_
  lower_name <- tryCatch(fit$step_3$lower_ci_string, error = function(e) NULL) %||% "2.5%"
  upper_name <- tryCatch(fit$step_3$upper_ci_string, error = function(e) NULL) %||% "97.5%"
  data.frame(
    recommended_n = if (reached) recommended_n else NA_real_, backend_recommended_n = recommended_n,
    backend_n_lower = if (lower_name %in% names(rec)) as.numeric(rec[[lower_name]]) else NA_real_,
    backend_n_upper = if (upper_name %in% names(rec)) as.numeric(rec[[upper_name]]) else NA_real_,
    target_probability = target_probability, achieved_probability = achieved,
    fitted_probability = evaluate(point_y), probability_source = curve_source,
    probability_comparison = if (decreasing) "<=" else ">=",
    smallest_evaluated_n = lower, largest_evaluated_n = upper,
    at_lower_boundary = is.finite(recommended_n) && recommended_n == lower,
    at_upper_boundary = is.finite(recommended_n) && recommended_n == upper,
    reached = reached, stringsAsFactors = FALSE
  )
}

quicknet_power_powerly_summary <- function(fit, target_metric, target_value) {
  sample_sizes <- tryCatch(fit$range$partition, error = function(e) NULL)
  statistics <- tryCatch(as.numeric(fit$step_1$statistics), error = function(e) NULL)
  if (is.null(sample_sizes) || is.null(statistics)) {
    return(data.frame())
  }
  if (length(sample_sizes) != length(statistics)) stop("Powerly returned mismatched sample-size and statistic lengths.", call. = FALSE)
  result <- data.frame(sample_size = sample_sizes, target_metric = target_metric,
    target_value = target_value, achieved_probability = statistics, stringsAsFactors = FALSE)
  measures <- tryCatch(fit$step_1$measures, error = function(e) NULL)
  if (is.matrix(measures) && ncol(measures) == nrow(result)) {
    # Source StepOne replaces NA recovery values with zero before exposing these.
    result$replications <- nrow(measures)
    result$finite_metric_replications <- colSums(is.finite(measures))
    result$achieved_replications <- colSums(measures >= target_value, na.rm = TRUE)
    uncertainty <- lapply(seq_len(ncol(measures)), function(i)
      quicknet_power_binomial(result$achieved_replications[[i]], sum(!is.na(measures[, i]))))
    result$probability_mcse <- vapply(uncertainty, `[[`, numeric(1), "mcse")
    result$probability_ci_lower <- vapply(uncertainty, `[[`, numeric(1), "lower")
    result$probability_ci_upper <- vapply(uncertainty, `[[`, numeric(1), "upper")
  }
  result
}

quicknet_power_object <- function(method,
                                  model,
                                  settings,
                                  true_network,
                                  results,
                                  summary,
                                  recommendation,
                                  fit,
                                  report,
                                  generating_network = NULL) {
  structure(
    list(
      method = method,
      model = model,
      settings = settings,
      true_network = true_network,
      generating_network = generating_network,
      results = results,
      summary = summary,
      recommendation = recommendation,
      fit = fit,
      report = report
    ),
    class = "quicknet_power"
  )
}

quicknet_power_validate_target <- function(metric, value, probability) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) stop("target_value must be a finite number.", call. = FALSE)
  lower <- if (metric %in% c("mcc", "edge_weight_correlation")) -1 else 0
  upper <- 1
  if (value < lower || value > upper) stop("target_value for ", metric, " must be in [", lower, ", ", upper, "].", call. = FALSE)
  if (!is.numeric(probability) || length(probability) != 1L || !is.finite(probability) || probability < 0 || probability > 1)
    stop("target_probability must be a finite number in [0, 1].", call. = FALSE)
  invisible(TRUE)
}

quicknet_power_validate_true_matrix <- function(x) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) != ncol(x) || nrow(x) < 2 ||
      any(!is.finite(x)) || !isTRUE(all.equal(unname(x), unname(t(x)), tolerance = 1e-12)) || any(diag(x) != 0))
    stop("model_matrix must be a finite symmetric matrix with at least two nodes and a zero diagonal.", call. = FALSE)
  if (min(eigen(diag(nrow(x)) - x, symmetric = TRUE, only.values = TRUE)$values) <= 0)
    stop("model_matrix must define a positive-definite precision matrix I - model_matrix.", call. = FALSE)
  invisible(TRUE)
}

quicknet_power_target_defined <- function(truth, metric, threshold) {
  values <- truth[upper.tri(truth)]
  selected <- abs(values) > threshold
  switch(metric,
    sensitivity = any(selected), specificity = any(!selected),
    mcc = any(selected) && any(!selected),
    edge_weight_correlation = length(values) > 1 && stats::sd(values) > 0,
    FALSE)
}
