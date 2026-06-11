#' Network power and sample size planning
#'
#' @param method Planning method. \code{"monte_carlo"} runs a transparent
#' simulation for Gaussian graphical models. \code{"powerly"} delegates GGM
#' planning to \code{powerly::powerly()}.
#' @param nodes Number of network nodes.
#' @param density Expected proportion of nonzero edges in the assumed true
#' network.
#' @param positive Proportion of nonzero edges with positive signs.
#' @param edge_strength Absolute nonzero edge-weight range used for the assumed
#' true partial-correlation network.
#' @param sample_sizes Candidate sample sizes for \code{method = "monte_carlo"}.
#' @param replications Monte Carlo replications per candidate sample size.
#' @param target_metric Metric used for the power criterion. Supported values
#' are \code{"sensitivity"}, \code{"specificity"}, \code{"mcc"},
#' \code{"edge_weight_correlation"}, and \code{"rmse"}.
#' @param target_value Target metric value.
#' @param target_probability Required proportion of replications that should
#' achieve the target value.
#' @param gamma EBIC tuning parameter used by EBICglasso.
#' @param estimator Network estimator used in the Monte Carlo loop.
#' @param seed Random seed.
#' @param powerly_args Optional named list passed to \code{powerly::powerly()}.
#' @param threshold Absolute threshold used to define selected edges.
#' @param ... Arguments passed from \code{SampleSize()} to
#' \code{NetworkPower()}.
#'
#' @return A \code{quicknet_power} object. Results depend on the assumed true
#' network and simulation design; they should be reported as design-based
#' planning evidence, not as a universal sample size rule.
#' @export
NetworkPower <- function(method = c("monte_carlo", "powerly"),
                         nodes = 8,
                         density = 0.30,
                         positive = 0.70,
                         edge_strength = c(0.15, 0.45),
                         sample_sizes = c(100, 200, 400),
                         replications = 100,
                         target_metric = c("sensitivity", "specificity", "mcc", "edge_weight_correlation", "rmse"),
                         target_value = 0.60,
                         target_probability = 0.80,
                         gamma = 0.50,
                         estimator = c("EBICglasso", "partial", "correlation"),
                         seed = 20260502,
                         powerly_args = list(),
                         threshold = 1e-10) {
  method <- match.arg(method)
  target_metric <- match.arg(target_metric)
  estimator <- match.arg(estimator)
  quicknet_validate_input(
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
      powerly_args = powerly_args
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
  if (!is.null(x$recommendation$recommended_n)) {
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
                                       threshold) {
  quicknet_power_validate_design(nodes, density, positive, edge_strength, sample_sizes, replications)
  set.seed(seed)
  true_network <- quicknet_power_true_network(nodes, density, positive, edge_strength)
  covariance <- quicknet_power_covariance_from_partial(true_network)

  rows <- vector("list", length(sample_sizes) * replications)
  row_index <- 1L
  for (sample_size in sample_sizes) {
    for (replication in seq_len(replications)) {
      estimated <- tryCatch(
        quicknet_power_simulate_estimate(
          sample_size = sample_size,
          covariance = covariance,
          estimator = estimator,
          gamma = gamma
        ),
        error = function(e) NULL
      )
      if (is.null(estimated)) {
        metric <- quicknet_power_empty_metric(sample_size, replication, gamma, estimator)
      } else {
        metric <- quicknet_power_recovery_metrics(true_network, estimated, threshold = threshold)
        metric$sample_size <- sample_size
        metric$replication <- replication
        metric$gamma <- gamma
        metric$estimator <- estimator
        metric$estimated_nonzero_edges <- sum(abs(estimated[upper.tri(estimated)]) > threshold, na.rm = TRUE)
        metric$failed <- FALSE
      }
      rows[[row_index]] <- metric
      row_index <- row_index + 1L
    }
  }

  results <- do.call(rbind, rows)
  summary <- quicknet_power_summary(results, target_metric, target_value)
  recommendation <- quicknet_power_recommend(summary, target_probability)
  settings <- list(
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
  report <- quicknet_power_report_text(recommendation, target_metric, target_value, target_probability)
  quicknet_power_object(
    method = "monte_carlo",
    model = "ggm",
    settings = settings,
    true_network = true_network,
    results = results,
    summary = summary,
    recommendation = recommendation,
    fit = NULL,
    report = report
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
                                   powerly_args) {
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
  set.seed(seed)
  defaults <- list(
    range_lower = 100,
    range_upper = 500,
    samples = 5,
    replications = 100,
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
    boots = 1000,
    lower_ci = 0.025,
    upper_ci = 0.975,
    verbose = FALSE
  )
  args <- utils::modifyList(defaults, powerly_args)
  fit <- do.call(powerly::powerly, args)
  recommendation <- quicknet_power_powerly_recommendation(fit)
  summary <- quicknet_power_powerly_summary(fit, target_metric, target_value)
  settings <- c(args, list(seed = seed, target_metric = target_metric, target_probability = target_probability))
  report <- quicknet_power_report_text(recommendation, target_metric, target_value, target_probability)
  quicknet_power_object(
    method = "powerly",
    model = "ggm",
    settings = settings,
    true_network = NULL,
    results = NULL,
    summary = summary,
    recommendation = recommendation,
    fit = fit,
    report = report
  )
}

quicknet_power_validate_design <- function(nodes, density, positive, edge_strength, sample_sizes, replications) {
  if (nodes < 3) stop("nodes must be at least 3.", call. = FALSE)
  if (density <= 0 || density > 1) stop("density must be in (0, 1].", call. = FALSE)
  if (positive < 0 || positive > 1) stop("positive must be in [0, 1].", call. = FALSE)
  if (length(edge_strength) != 2 || any(edge_strength <= 0) || edge_strength[[1]] > edge_strength[[2]]) {
    stop("edge_strength must be a positive length-2 range.", call. = FALSE)
  }
  if (any(sample_sizes < 5)) stop("sample_sizes must be at least 5.", call. = FALSE)
  if (replications < 1) stop("replications must be at least 1.", call. = FALSE)
}

quicknet_power_true_network <- function(nodes, density, positive, edge_strength) {
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
  mat <- quicknet_power_make_pd_partial(mat)
  diag(mat) <- 0
  mat
}

quicknet_power_make_pd_partial <- function(partial) {
  scale_factor <- 1
  repeat {
    precision <- diag(nrow(partial))
    precision[upper.tri(precision)] <- -partial[upper.tri(partial)] * scale_factor
    precision[lower.tri(precision)] <- t(precision)[lower.tri(precision)]
    min_eigen <- min(eigen(precision, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eigen > 1e-6) {
      return(partial * scale_factor)
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
  cor_mat <- quicknet_make_positive_definite(cor_mat)
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
  tp <- sum(true_edge & estimated_edge)
  fn <- sum(true_edge & !estimated_edge)
  tn <- sum(!true_edge & !estimated_edge)
  fp <- sum(!true_edge & estimated_edge)
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
    gamma = gamma,
    estimator = estimator,
    estimated_nonzero_edges = NA_integer_,
    failed = TRUE,
    stringsAsFactors = FALSE
  )
}

quicknet_power_summary <- function(results, target_metric, target_value) {
  metric_names <- c("sensitivity", "specificity", "mcc", "edge_weight_correlation", "rmse")
  rows <- lapply(sort(unique(results$sample_size)), function(sample_size) {
    subset <- results[results$sample_size == sample_size, , drop = FALSE]
    out <- data.frame(
      sample_size = sample_size,
      replications = nrow(subset),
      failed_replications = sum(subset$failed %in% TRUE, na.rm = TRUE),
      achieved_probability = quicknet_power_achieved_probability(subset[[target_metric]], target_metric, target_value),
      mean_estimated_nonzero_edges = mean(subset$estimated_nonzero_edges, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    for (metric in metric_names) {
      out[[paste0("mean_", metric)]] <- mean(subset[[metric]], na.rm = TRUE)
      out[[paste0("sd_", metric)]] <- stats::sd(subset[[metric]], na.rm = TRUE)
    }
    out
  })
  do.call(rbind, rows)
}

quicknet_power_achieved_probability <- function(values, metric, target_value) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NA_real_)
  if (metric == "rmse") {
    mean(values <= target_value)
  } else {
    mean(values >= target_value)
  }
}

quicknet_power_recommend <- function(summary, target_probability) {
  eligible <- summary[summary$achieved_probability >= target_probability, , drop = FALSE]
  data.frame(
    recommended_n = if (nrow(eligible) > 0) min(eligible$sample_size) else NA_real_,
    target_probability = target_probability,
    reached = nrow(eligible) > 0,
    stringsAsFactors = FALSE
  )
}

quicknet_power_powerly_recommendation <- function(fit) {
  rec <- tryCatch(fit$recommendation, error = function(e) NULL)
  recommended_n <- if (!is.null(rec)) {
    if ("50%" %in% names(rec)) as.numeric(rec[["50%"]]) else as.numeric(rec[[1]])
  } else {
    NA_real_
  }
  data.frame(
    recommended_n = recommended_n,
    target_probability = NA_real_,
    reached = is.finite(recommended_n),
    stringsAsFactors = FALSE
  )
}

quicknet_power_powerly_summary <- function(fit, target_metric, target_value) {
  sample_sizes <- tryCatch(fit$range$partition, error = function(e) NULL)
  statistics <- tryCatch(as.numeric(fit$step_1$statistics), error = function(e) NULL)
  if (is.null(sample_sizes) || is.null(statistics)) {
    return(data.frame())
  }
  data.frame(
    sample_size = sample_sizes,
    target_metric = target_metric,
    target_value = target_value,
    achieved_probability = statistics,
    stringsAsFactors = FALSE
  )
}

quicknet_power_report_text <- function(recommendation, target_metric, target_value, target_probability) {
  if (isTRUE(recommendation$reached[[1]])) {
    paste0(
      "Recommended N = ", recommendation$recommended_n[[1]],
      " to achieve P(", target_metric, " reaches ", target_value, ") >= ",
      target_probability,
      " under the specified simulation design."
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
                                  report) {
  structure(
    list(
      method = method,
      model = model,
      settings = settings,
      true_network = true_network,
      results = results,
      summary = summary,
      recommendation = recommendation,
      fit = fit,
      report = report
    ),
    class = "quicknet_power"
  )
}
