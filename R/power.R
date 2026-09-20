#' Network power and sample size planning
#'
#' @param method Planning method. \code{"monte_carlo"} runs a transparent
#' simulation for Gaussian graphical models. \code{"powerly"} delegates GGM
#' planning to \code{powerly::powerly()}.
#' @param nodes Number of network nodes. Required for powerly; defaults to 8
#'   for quickNet's Monte Carlo design. With powerly's model_matrix in ...,
#'   nodes and density are inferred and no generator arguments are required.
#' @param density Expected proportion of nonzero edges in the assumed true
#' network. Required for powerly; Monte Carlo defaults to 0.30.
#' @param positive Proportion of nonzero edges with positive signs.
#' @param edge_strength Absolute nonzero edge-weight range used for the assumed
#' true partial-correlation network.
#' @param sample_sizes Candidate sample sizes for \code{method = "monte_carlo"}.
#' If \code{NULL}, an adaptive grid is generated from \code{nodes}.
#' @param replications Replications per sample size. NULL inherits 30 for
#'   powerly and uses 100 for the quickNet Monte Carlo design.
#' @param target_metric Metric used for the power criterion. Supported values
#' are \code{"sensitivity"}, \code{"specificity"}, \code{"mcc"},
#' \code{"edge_weight_correlation"}, and \code{"rmse"}.
#' @param target_value Target metric value: [0,1] for sensitivity/specificity,
#'   [-1,1] for MCC/edge-weight correlation, and nonnegative for RMSE.
#' @param target_probability Required proportion of replications that should
#' achieve the target value.
#' @param gamma EBIC hyperparameter in [0,1]. NULL selects 0.5 for
#'   Monte Carlo EBICglasso estimation. Ignored for partial/correlation
#'   estimators and for method = 'powerly'. Powerly's internal GGM estimator
#'   uses its own gamma default (0.5), recorded separately as backend_gamma;
#'   the top-level gamma does not configure it. Inactive gamma is stored as NULL in settings
#'   and NA in Monte Carlo result rows.
#' @param estimator Network estimator used in the Monte Carlo loop. Correlation
#'   is evaluated against the true marginal correlation matrix; partial and
#'   EBICglasso are evaluated against the generating partial correlations.
#' @param seed Optional random seed. NULL draws from the current RNG stream.
#' @param powerly_args Legacy optional list. Named arguments through ... are
#'   preferred and override entries in this list; no parameter object is needed.
#' @param threshold Absolute threshold used to define selected edges.
#' @param ... Named powerly controls, including required range_lower and
#'   range_upper, e.g. \code{range_lower = 100, range_upper = 500, samples = 30}.
#'   SampleSize forwards its arguments to NetworkPower.
#'
#' @details Each call conditions on one fixed true network. Monte Carlo summaries
#'   include failed fits, undefined target metrics, Monte Carlo standard errors
#'   and pointwise exact-binomial 95% intervals. Failed or undefined target values
#'   count as not achieved. The candidate N uses the point estimate; its interval
#'   is not adjusted for selecting among candidate sample sizes. Validate a chosen
#'   candidate with independent simulations. The actual generating edge strengths
#'   and any positive-definiteness scaling are recorded.
#'
#'   Powerly's bootstrap-median curve determines its median N; the source curve
#'   and sample-size interval are retained. Its source GGM generator uses five
#'   ordinal levels by default and replaces undefined recovery measures with zero.
#'   Use powerly::validate() on the returned fit component for native validation.
#' @return A \code{quicknet_power} object. Results depend on the assumed true
#' network and simulation design; true_network is the target of recovery and
#' generating_network contains the generating partial-correlation matrix.
#' Results should be reported as design-based
#' planning evidence, not as a universal sample size rule.
#' @export
NetworkPower <- function(method = c("monte_carlo", "powerly"),
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
                         estimator = c("EBICglasso", "partial", "correlation"),
                         seed = NULL,
                         powerly_args = list(),
                         threshold = 1e-10,
                         ...) {
  supplied <- names(match.call())
  method <- match.arg(method)
  if (method == "powerly" && any(c("estimator", "threshold") %in% supplied)) stop("estimator and threshold are controls for the Monte Carlo branch.", call. = FALSE)
  dots <- list(...)
  if (method == "monte_carlo" && (length(dots) || length(powerly_args))) stop("Additional backend arguments apply only to method = 'powerly'.", call. = FALSE)
  native_args <- quicknet_merge_args(powerly_args, dots)
  supplied_matrix <- if (method == "powerly") native_args$model_matrix else NULL
  if (!is.null(supplied_matrix)) {
    quicknet_power_validate_true_matrix(supplied_matrix)
    nodes <- nrow(supplied_matrix)
    density <- mean(supplied_matrix[upper.tri(supplied_matrix)] != 0)
  } else if (method == "powerly" && (is.null(nodes) || is.null(density))) {
    stop("nodes and density must be specified for powerly's network generator, unless model_matrix is supplied.", call. = FALSE)
  }
  if (method == "powerly" && !is.null(sample_sizes)) stop("Use range_lower and range_upper for powerly; sample_sizes applies only to Monte Carlo planning.", call. = FALSE)
  nodes <- nodes %||% 8
  density <- density %||% 0.30
  positive <- positive %||% if (method == "powerly") 0.9 else 0.7
  edge_strength <- edge_strength %||% if (method == "powerly") c(0.5, 1) else c(0.15, 0.45)
  replications <- replications %||% if (method == "powerly") 30 else 100
  target_metric <- target_metric %||% if (method == "powerly") "sensitivity" else "mcc"
  target_metric <- match.arg(target_metric, c("mcc", "sensitivity", "specificity", "edge_weight_correlation", "rmse"))
  estimator <- match.arg(estimator)
  minimum_nodes <- if (method == "powerly") 2L else 3L
  if (!quicknet_is_positive_integer(nodes) || nodes < minimum_nodes) {
    stop("nodes must be an integer of at least ", minimum_nodes, ".", call. = FALSE)
  }
  if (!is.numeric(target_probability) || length(target_probability) != 1 ||
      !is.finite(target_probability) || target_probability < 0 || target_probability > 1) {
    stop("target_probability must be a finite number in [0, 1].", call. = FALSE)
  }
  quicknet_power_validate_target(target_metric, target_value, target_probability)
  gamma <- quicknet_resolve_gamma(
    if (method == "monte_carlo") estimator else "powerly", gamma
  )
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      !is.finite(threshold) || threshold < 0) {
    stop("threshold must be a non-negative finite number.", call. = FALSE)
  }
  sample_sizes <- quicknet_power_resolve_sample_sizes(sample_sizes, nodes)
  if (method == "monte_carlo") quicknet_validate_input(
    model = "power",
    nodes = nodes,
    density = density,
    sample_sizes = sample_sizes,
    replications = replications,
    warn = FALSE
  )
  if (method == "powerly") {
    return(quicknet_power_powerly(
      nodes = nodes,
      density = density,
      positive = positive,
      edge_strength = edge_strength,
      target_metric = target_metric,
      target_value = target_value,
      target_probability = target_probability,
      seed = seed,
      powerly_args = native_args,
      replications = replications
    ))
  }

  quicknet_power_monte_carlo(
    nodes = nodes,
    density = density,
    positive = positive,
    edge_strength = edge_strength,
    sample_sizes = sample_sizes,
    replications = replications,
    target_metric = target_metric,
    target_value = target_value,
    target_probability = target_probability,
    gamma = gamma,
    estimator = estimator,
    seed = seed,
    threshold = threshold
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
  object$summary
}

#' Plot network power planning results
#'
#' @param x A \code{quicknet_power} object.
#' @param type Plot type. \code{"power"} shows the target-achievement
#' probability across candidate sample sizes. \code{"metric"} shows the mean
#' target metric across candidate sample sizes.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @export
plot.quicknet_power <- function(x, type = c("power", "metric"), ...) {
  if (!inherits(x, "quicknet_power")) {
    stop("x must be a quicknet_power object.", call. = FALSE)
  }
  type <- match.arg(type)
  if (x$method != "monte_carlo") {
    stop("plot.quicknet_power() currently supports method = 'monte_carlo'. Use the raw powerly object for powerly-specific plots.", call. = FALSE)
  }
  df <- x$summary
  metric <- x$settings$target_metric
  if (type == "power") {
    ggplot2::ggplot(df, ggplot2::aes(x = sample_size, y = achieved_probability)) +
      ggplot2::geom_line(color = "#365f7f", linewidth = 0.8) +
      ggplot2::geom_point(color = "#365f7f", size = 2.2) +
      ggplot2::geom_hline(yintercept = x$settings$target_probability, linetype = "dashed", color = "#9a4d3f") +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        title = "Network sample size planning",
        subtitle = paste0("P(", metric, " reaches target)"),
        x = "Candidate sample size",
        y = "Target-achievement probability",
        caption = "Simulation-based planning; results depend on the assumed true network."
      ) +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    mean_col <- paste0("mean_", metric)
    df$plot_metric <- df[[mean_col]]
    ggplot2::ggplot(df, ggplot2::aes(x = sample_size, y = plot_metric)) +
      ggplot2::geom_line(color = "#3f7f5f", linewidth = 0.8) +
      ggplot2::geom_point(color = "#3f7f5f", size = 2.2) +
      ggplot2::labs(
        title = "Mean recovery metric by sample size",
        subtitle = metric,
        x = "Candidate sample size",
        y = metric,
        caption = "Simulation-based planning; results depend on the assumed true network."
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }
}

quicknet_power_monte_carlo <- function(nodes,
                                       density,
                                       positive,
                                       edge_strength,
                                       sample_sizes,
                                       replications,
                                       target_metric,
                                       target_value,
                                       target_probability,
                                       gamma,
                                       estimator,
                                       seed,
                                       threshold,
                                       true_network = NULL) {
  quicknet_power_validate_design(nodes, density, positive, edge_strength, sample_sizes, replications)
  quicknet_power_validate_target(target_metric, target_value, target_probability)
  if (estimator == "partial" && any(sample_sizes <= nodes)) {
    stop("Unregularized partial-correlation estimation requires every sample size to exceed nodes.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  generated <- if (is.null(true_network)) {
    quicknet_power_true_network(nodes, density, positive, edge_strength, details = TRUE)
  } else {
    quicknet_power_validate_true_matrix(true_network)
    list(graph = true_network, scale_factor = 1)
  }
  generating_network <- generated$graph
  covariance <- quicknet_power_covariance_from_partial(generating_network)
  true_network <- if (estimator == "correlation") covariance else generating_network
  diag(true_network) <- 0
  estimand <- if (estimator == "correlation") "marginal_correlation" else "partial_correlation"

  rows <- vector("list", length(sample_sizes) * replications)
  row_index <- 1L
  for (sample_size in sample_sizes) {
    for (replication in seq_len(replications)) {
      attempted <- tryCatch({
        estimated <- quicknet_power_simulate_estimate(sample_size, covariance, estimator, gamma)
        if (!is.matrix(estimated) || !identical(dim(estimated), dim(true_network)) ||
            any(!is.finite(estimated))) stop("Estimator returned an invalid network.")
        metric <- quicknet_power_recovery_metrics(true_network, estimated, threshold)
        metric$sample_size <- sample_size
        metric$replication <- replication
        metric$gamma <- gamma %||% NA_real_
        metric$estimator <- estimator
        metric$estimated_nonzero_edges <- sum(abs(estimated[upper.tri(estimated)]) > threshold)
        metric$failed <- FALSE
        metric$error_message <- NA_character_
        metric
      }, error = function(e) e)
      if (inherits(attempted, "error")) {
        metric <- quicknet_power_empty_metric(sample_size, replication, gamma, estimator)
        metric$error_message <- conditionMessage(attempted)
      } else metric <- attempted
      rows[[row_index]] <- metric
      row_index <- row_index + 1L
    }
  }

  results <- do.call(rbind, rows)
  quicknet_check_failed_iterations(results$failed, "Monte Carlo replications")
  summary <- quicknet_power_summary(results, target_metric, target_value)
  recommendation <- quicknet_power_recommend(summary, target_probability)
  generating_values <- generating_network[upper.tri(generating_network)]
  selected <- generating_values != 0
  settings <- list(
    nodes = nodes, density = density, positive = positive, edge_strength = edge_strength,
    sample_sizes = sample_sizes, replications = replications, target_metric = target_metric,
    target_value = target_value, target_probability = target_probability, gamma = gamma,
    estimator = estimator, seed = seed, threshold = threshold, estimand = estimand,
    target_defined_for_truth = quicknet_power_target_defined(true_network, target_metric, threshold),
    data_generation = "continuous_multivariate_normal",
    conditioning = "one_fixed_generating_network_per_call",
    positive_definite_scale = generated$scale_factor,
    generated_density = mean(selected), generated_positive = mean(generating_values[selected] > 0),
    generated_edge_strength = range(abs(generating_values[selected])),
    confidence_level = 0.95, confidence_interval = "pointwise_exact_binomial",
    denominator_policy = "all_replications; failed_or_undefined_target_counts_as_not_achieved",
    recommendation_rule = "smallest_candidate_with_finite_target_metrics_and_point_probability_at_least_target"
  )
  report <- paste0(quicknet_power_report_text(recommendation, target_metric, target_value, target_probability),
    " Estimand: ", estimand, "; probabilities are conditional on one fixed generating network.",
    " Generated edge density = ", signif(settings$generated_density, 3),
    "; actual nonzero partial-correlation magnitudes = [",
    paste(signif(settings$generated_edge_strength, 3), collapse = ", "),
    "]; positive-definite scaling factor = ", signif(generated$scale_factor, 4), ".",
    " Failed fits: ", sum(results$failed), "; undefined target metrics after successful fits: ",
    sum(summary$undefined_target_replications), ".")
  quicknet_power_object(
    method = "monte_carlo", model = "ggm", settings = settings, true_network = true_network,
    results = results, summary = summary, recommendation = recommendation, fit = NULL,
    report = report, generating_network = generating_network
  )
}

quicknet_power_powerly <- function(nodes,
                                   density,
                                   positive,
                                   edge_strength,
                                   target_metric,
                                   target_value,
                                   target_probability,
                                   seed,
                                   powerly_args,
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
  powerly_args <- quicknet_backend_args(powerly_args, powerly::powerly,
    reserved = c("model"), extra = c("nodes", "density", "positive", "constant", "range"))
  args <- quicknet_merge_args(defaults, powerly_args)
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
           "). This is the source software's interpolated estimate; validate it with powerly::validate(fit$fit).")
  } else if (identical(settings$target_defined_for_truth, FALSE)) {
    "The requested recovery metric is undefined for this true network. Powerly replaces undefined measures with zero; its numerical recommendation cannot establish target attainment."
  } else {
    "Powerly's bootstrap-median curve did not meet the requested probability at the returned sample size. Extend the evaluated range or increase simulation precision."
  }
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

quicknet_power_validate_design <- function(nodes, density, positive, edge_strength, sample_sizes, replications) {
  if (!quicknet_is_positive_integer(nodes) || nodes < 3) {
    stop("nodes must be an integer of at least 3.", call. = FALSE)
  }
  if (!is.numeric(density) || length(density) != 1 || !is.finite(density) ||
      density <= 0 || density > 1) {
    stop("density must be a finite number in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(positive) || length(positive) != 1 || !is.finite(positive) ||
      positive < 0 || positive > 1) {
    stop("positive must be a finite number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(edge_strength) || length(edge_strength) != 2 ||
      any(!is.finite(edge_strength)) || any(edge_strength <= 0) ||
      edge_strength[[1]] > edge_strength[[2]]) {
    stop("edge_strength must be a positive length-2 range.", call. = FALSE)
  }
  if (!is.numeric(sample_sizes) || length(sample_sizes) == 0 ||
      any(!is.finite(sample_sizes)) ||
      any(vapply(sample_sizes, function(x) !quicknet_is_positive_integer(x), logical(1))) ||
      any(sample_sizes < 5)) {
    stop("sample_sizes must contain positive integers of at least 5.", call. = FALSE)
  }
  if (!quicknet_is_positive_integer(replications)) {
    stop("replications must be a positive integer.", call. = FALSE)
  }
}

quicknet_power_resolve_sample_sizes <- function(sample_sizes, nodes) {
  if (!is.null(sample_sizes)) {
    if (!is.numeric(sample_sizes) || length(sample_sizes) == 0 ||
        any(!is.finite(sample_sizes)) ||
        any(vapply(sample_sizes, function(x) !quicknet_is_positive_integer(x), logical(1)))) {
      stop("sample_sizes must contain positive integers.", call. = FALSE)
    }
    return(sort(unique(as.integer(sample_sizes))))
  }
  quicknet_power_default_sample_sizes(nodes)
}

quicknet_power_default_sample_sizes <- function(nodes) {
  upper <- max(400L, as.integer(ceiling(nodes * 50 / 100) * 100))
  base_grid <- c(100L, 200L, 400L, 800L, 1200L, 1600L, 2400L, 3200L, 4800L, 6400L)
  grid <- base_grid[base_grid <= upper]
  sort(unique(c(grid, upper)))
}

quicknet_power_true_network <- function(nodes, density, positive, edge_strength, details = FALSE) {
  node_names <- paste0("V", seq_len(nodes))
  mat <- matrix(0, nodes, nodes, dimnames = list(node_names, node_names))
  edge_index <- which(upper.tri(mat), arr.ind = TRUE)
  edge_count <- max(1L, round(nrow(edge_index) * density))
  chosen <- edge_index[sample(seq_len(nrow(edge_index)), edge_count), , drop = FALSE]
  weights <- stats::runif(edge_count, edge_strength[[1]], edge_strength[[2]])
  signs <- ifelse(stats::runif(edge_count) <= positive, 1, -1)
  weights <- weights * signs
  for (i in seq_len(edge_count)) {
    mat[chosen[i, "row"], chosen[i, "col"]] <- weights[[i]]
    mat[chosen[i, "col"], chosen[i, "row"]] <- weights[[i]]
  }
  adjusted <- quicknet_power_make_pd_partial(mat, details = TRUE)
  diag(adjusted$graph) <- 0
  if (details) adjusted else adjusted$graph
}

quicknet_power_make_pd_partial <- function(partial, details = FALSE) {
  scale_factor <- 1
  repeat {
    precision <- diag(nrow(partial))
    precision[upper.tri(precision)] <- -partial[upper.tri(partial)] * scale_factor
    precision[lower.tri(precision)] <- t(precision)[lower.tri(precision)]
    min_eigen <- min(eigen(precision, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eigen > 1e-6) {
      graph <- partial * scale_factor
      return(if (details) list(graph = graph, scale_factor = scale_factor) else graph)
    }
    scale_factor <- scale_factor * 0.90
    if (scale_factor < 0.05) {
      stop("Could not construct a positive-definite true network from the requested settings.", call. = FALSE)
    }
  }
}

quicknet_power_covariance_from_partial <- function(partial) {
  precision <- diag(nrow(partial))
  precision[upper.tri(precision)] <- -partial[upper.tri(partial)]
  precision[lower.tri(precision)] <- t(precision)[lower.tri(precision)]
  covariance <- solve(precision)
  covariance <- stats::cov2cor(covariance)
  colnames(covariance) <- rownames(covariance) <- colnames(partial)
  covariance
}

quicknet_power_simulate_estimate <- function(sample_size, covariance, estimator, gamma) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for Monte Carlo sample size planning.", call. = FALSE)
  }
  node_names <- colnames(covariance)
  simulated <- MASS::mvrnorm(n = sample_size, mu = rep(0, ncol(covariance)), Sigma = covariance)
  simulated <- as.data.frame(simulated)
  colnames(simulated) <- node_names
  cor_mat <- stats::cor(simulated, use = "pairwise.complete.obs")
  if (any(!is.finite(cor_mat))) stop("Simulation produced a non-finite correlation matrix.", call. = FALSE)
  if (estimator == "EBICglasso") {
    estimated <- suppressWarnings(suppressMessages(qgraph::EBICglasso(
      cor_mat,
      n = sample_size,
      gamma = gamma,
      verbose = FALSE
    )))
  } else if (estimator == "partial") {
    estimated <- quicknet_partial_cor(cor_mat)
  } else {
    estimated <- cor_mat
    diag(estimated) <- 0
  }
  estimated <- as.matrix(estimated)
  diag(estimated) <- 0
  colnames(estimated) <- rownames(estimated) <- node_names
  estimated
}

quicknet_power_recovery_metrics <- function(true_graph, estimated_graph, threshold) {
  true_values <- true_graph[upper.tri(true_graph)]
  estimated_values <- estimated_graph[upper.tri(estimated_graph)]
  true_edge <- abs(true_values) > threshold
  estimated_edge <- abs(estimated_values) > threshold
  # Products of integer edge counts overflow for even moderately sized networks.
  tp <- as.double(sum(true_edge & estimated_edge))
  fn <- as.double(sum(true_edge & !estimated_edge))
  tn <- as.double(sum(!true_edge & !estimated_edge))
  fp <- as.double(sum(!true_edge & estimated_edge))
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  data.frame(
    true_positive = tp,
    false_negative = fn,
    true_negative = tn,
    false_positive = fp,
    sensitivity = ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_),
    specificity = ifelse((tn + fp) > 0, tn / (tn + fp), NA_real_),
    mcc = ifelse(denominator > 0, (tp * tn - fp * fn) / denominator, NA_real_),
    edge_weight_correlation = suppressWarnings(stats::cor(true_values, estimated_values)),
    rmse = sqrt(mean((estimated_values - true_values)^2)),
    stringsAsFactors = FALSE
  )
}

quicknet_power_empty_metric <- function(sample_size, replication, gamma, estimator) {
  data.frame(
    true_positive = NA_integer_,
    false_negative = NA_integer_,
    true_negative = NA_integer_,
    false_positive = NA_integer_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    mcc = NA_real_,
    edge_weight_correlation = NA_real_,
    rmse = NA_real_,
    sample_size = sample_size,
    replication = replication,
    gamma = gamma %||% NA_real_,
    estimator = estimator,
    estimated_nonzero_edges = NA_integer_,
    failed = TRUE,
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
}

quicknet_power_summary <- function(results, target_metric, target_value) {
  metric_names <- c("sensitivity", "specificity", "mcc", "edge_weight_correlation", "rmse")
  rows <- lapply(sort(unique(results$sample_size)), function(sample_size) {
    subset <- results[results$sample_size == sample_size, , drop = FALSE]
    failed <- subset$failed %in% TRUE
    valid <- is.finite(subset[[target_metric]]) & !failed
    achieved <- valid & if (target_metric == "rmse") subset[[target_metric]] <= target_value else subset[[target_metric]] >= target_value
    uncertainty <- quicknet_power_binomial(sum(achieved), nrow(subset))
    out <- data.frame(
      sample_size = sample_size, replications = nrow(subset),
      failed_replications = sum(failed), valid_target_replications = sum(valid),
      undefined_target_replications = sum(!failed & !valid), achieved_replications = sum(achieved),
      achieved_probability = uncertainty$probability,
      probability_mcse = uncertainty$mcse, probability_ci_lower = uncertainty$lower,
      probability_ci_upper = uncertainty$upper,
      mean_estimated_nonzero_edges = quicknet_safe_mean(subset$estimated_nonzero_edges),
      stringsAsFactors = FALSE
    )
    for (metric in metric_names) {
      out[[paste0("mean_", metric)]] <- quicknet_safe_mean(subset[[metric]])
      out[[paste0("sd_", metric)]] <- quicknet_safe_sd(subset[[metric]])
    }
    out
  })
  do.call(rbind, rows)
}

quicknet_power_binomial <- function(successes, trials) {
  if (trials < 1) return(list(probability = NA_real_, mcse = NA_real_, lower = NA_real_, upper = NA_real_))
  probability <- successes / trials
  list(probability = probability, mcse = sqrt(probability * (1 - probability) / trials),
       lower = if (successes == 0) 0 else stats::qbeta(0.025, successes, trials - successes + 1),
       upper = if (successes == trials) 1 else stats::qbeta(0.975, successes + 1, trials - successes))
}

quicknet_power_achieved_probability <- function(values, metric, target_value) {
  if (length(values) == 0) return(NA_real_)
  valid <- is.finite(values)
  if (metric == "rmse") {
    mean(valid & values <= target_value)
  } else {
    mean(valid & values >= target_value)
  }
}

quicknet_power_recommend <- function(summary, target_probability) {
  eligible_index <- is.finite(summary$achieved_probability) &
    summary$achieved_probability >= target_probability
  if ("valid_target_replications" %in% names(summary)) eligible_index <- eligible_index & summary$valid_target_replications > 0
  eligible <- summary[eligible_index, , drop = FALSE]
  recommended_n <- if (nrow(eligible) > 0) min(eligible$sample_size) else NA_real_
  recommended_row <- if (is.finite(recommended_n)) {
    summary[summary$sample_size == recommended_n, , drop = FALSE]
  } else {
    summary[0, , drop = FALSE]
  }
  data.frame(
    recommended_n = recommended_n,
    target_probability = target_probability,
    achieved_probability = if (nrow(recommended_row) > 0) recommended_row$achieved_probability[[1]] else NA_real_,
    probability_mcse = if (nrow(recommended_row) > 0) recommended_row$probability_mcse %||% NA_real_ else NA_real_,
    probability_ci_lower = if (nrow(recommended_row) > 0) recommended_row$probability_ci_lower %||% NA_real_ else NA_real_,
    probability_ci_upper = if (nrow(recommended_row) > 0) recommended_row$probability_ci_upper %||% NA_real_ else NA_real_,
    lower_bound_supports_target = if (nrow(recommended_row) > 0 && "probability_ci_lower" %in% names(recommended_row))
      recommended_row$probability_ci_lower >= target_probability else NA,
    selection_rule = "point_estimate_on_evaluated_grid",
    smallest_evaluated_n = min(summary$sample_size, na.rm = TRUE),
    largest_evaluated_n = max(summary$sample_size, na.rm = TRUE),
    at_lower_boundary = is.finite(recommended_n) && recommended_n == min(summary$sample_size, na.rm = TRUE),
    at_upper_boundary = is.finite(recommended_n) && recommended_n == max(summary$sample_size, na.rm = TRUE),
    reached = is.finite(recommended_n),
    stringsAsFactors = FALSE
  )
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
  curve_y <- if (has_median) ci[, "50%"] else point_y
  curve_source <- if (has_median) "bootstrap_median_curve" else "point_curve_legacy_fallback"
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

quicknet_power_report_text <- function(recommendation, target_metric, target_value, target_probability) {
  if (isTRUE(recommendation$reached[[1]])) {
    boundary_note <- if (isTRUE(recommendation$at_lower_boundary[[1]])) {
      " This is the smallest evaluated candidate; smaller sample sizes were not tested."
    } else if (isTRUE(recommendation$at_upper_boundary[[1]])) {
      " This is the largest evaluated candidate; consider extending the sample-size grid."
    } else {
      ""
    }
    paste0(
      "Smallest evaluated N meeting the criterion = ", recommendation$recommended_n[[1]],
      " to achieve P(", target_metric, " reaches ", target_value, ") >= ",
      target_probability,
      " under the specified simulation design, using the point estimate.",
      if ("probability_ci_lower" %in% names(recommendation)) paste0(
        " Pointwise exact-binomial 95% interval: [", signif(recommendation$probability_ci_lower[[1]], 3),
        ", ", signif(recommendation$probability_ci_upper[[1]], 3), "].",
        if (isTRUE(recommendation$lower_bound_supports_target[[1]])) " Its lower limit supports the target."
        else " Its lower limit does not establish target attainment."),
      " This interval is conditional on the fixed network and is not adjusted for selecting N across the grid; verify the candidate using independent simulations.",
      boundary_note
    )
  } else {
    paste0(
      "No candidate sample size reached P(", target_metric, " reaches ", target_value, ") >= ",
      target_probability,
      " under the specified simulation design."
    )
  }
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
  upper <- if (metric == "rmse") Inf else 1
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
    rmse = TRUE, FALSE)
}
