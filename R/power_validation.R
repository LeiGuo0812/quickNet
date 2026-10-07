#' Independently validate a native powerly sample size analysis
#'
#' @param plan A quicknet_power result from NetworkPower(method = "powerly").
#' @param replications Number of independent simulations. NULL inherits the
#'   installed powerly::validate default (3000 in powerly 1.10.0).
#' @param sample Sample size to validate. NULL uses the native median candidate
#'   only if its bootstrap-median curve attained the requested target.
#' @param seed Optional seed for new validation simulations. Use a different
#'   seed from the search; NULL continues the current RNG stream.
#' @param ... Native validation controls: cores, cluster_type and verbose.
#' @return A quicknet_power_validation object with the unmodified native
#'   Validation object in fit, raw recovery measures, numerical summary and
#'   text report. plot dispatches to the native validation plot.
#' @details This directly calls powerly::validate. The native undefined-value
#'   policy is retained. Additional exact-binomial intervals quantify simulation
#'   error at the chosen N; they do not quantify uncertainty in the assumed
#'   network or establish power for a null-hypothesis test. An interval crossing
#'   the target probability leaves attainment uncertain. Search convergence and
#'   validation attainment are distinct evidence.
#'   Native validation always tests attainment with >=, including when a native
#'   decreasing search uses <= for its curve crossing. Both comparison directions
#'   are recorded; the usual sample-size design uses increasing = TRUE.
#' @references Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2023).
#'   A general Monte Carlo method for sample size analysis in the context of
#'   network models. \doi{10.1037/met0000555}.
#' @export
ValidateNetworkPower <- function(plan, replications = NULL, sample = NULL,
                                 seed = NULL, ...) {
  if (!inherits(plan, "quicknet_power") || !identical(plan$method, "powerly") ||
      !inherits(plan$fit, "Method")) {
    stop("plan must be a native powerly result from NetworkPower(method = 'powerly').", call. = FALSE)
  }
  if (!requireNamespace("powerly", quietly = TRUE)) {
    stop("Package 'powerly' is required for independent validation.", call. = FALSE)
  }
  replications <- replications %||% quicknet_backend_default(powerly::validate, "replications")
  if (!quicknet_is_positive_integer(replications)) stop("replications must be a positive integer.", call. = FALSE)
  if (is.null(sample) && !isTRUE(plan$recommendation$reached[[1L]])) {
    stop("No attained sample-size candidate is available; supply an explicit sample to validate or extend the search.", call. = FALSE)
  }
  if (!is.null(sample) && !quicknet_is_positive_integer(sample)) stop("sample must be a positive integer.", call. = FALSE)
  dots <- quicknet_backend_args(list(...), powerly::validate,
    reserved = c("method", "replications", "sample"))
  args <- c(list(method = plan$fit, replications = replications, sample = sample), dots)
  if (!is.null(seed)) set.seed(seed)
  fit <- do.call(powerly::validate, args)
  measures <- as.numeric(fit$measures)
  target_value <- plan$fit$step_1$measure_value
  target_probability <- plan$fit$step_1$statistic_value
  successes <- sum(is.finite(measures) & measures >= target_value)
  uncertainty <- quicknet_power_binomial(successes, length(measures))
  defined <- !identical(plan$settings$target_defined_for_truth, FALSE)
  search_comparison <- plan$recommendation$probability_comparison %||% ">="
  summary <- data.frame(
    sample_size = as.numeric(fit$sample), replications = length(measures),
    achieved_replications = successes, achieved_probability = uncertainty$probability,
    native_probability = as.numeric(fit$statistic), probability_mcse = uncertainty$mcse,
    probability_ci_lower = uncertainty$lower, probability_ci_upper = uncertainty$upper,
    lower_bound_supports_target = defined && uncertainty$lower >= target_probability,
    point_estimate_reaches_target = defined && uncertainty$probability >= target_probability,
    native_percentile_value = as.numeric(fit$percentile_value),
    target_value = target_value, target_probability = target_probability,
    target_defined_for_truth = defined,
    search_probability_comparison = search_comparison,
    validation_probability_comparison = ">=",
    stringsAsFactors = FALSE
  )
  status <- if (!defined) "undefined_target" else if (summary$lower_bound_supports_target) {
    "supported"
  } else if (uncertainty$upper < target_probability) "below_target" else "uncertain"
  settings <- list(
    backend = "powerly::validate", backend_version = as.character(utils::packageVersion("powerly")),
    replications = replications, sample = as.numeric(fit$sample), seed = seed,
    search_seed = plan$settings$seed, target_metric = plan$settings$target_metric,
    target_value = target_value, target_probability = target_probability,
    backend_args = dots, algorithm_converged = plan$settings$algorithm_converged,
    denominator_policy = "native_powerly_replaces_NA_measures_with_zero",
    confidence_interval = "exact_binomial_95_percent_conditional_on_assumed_network",
    search_probability_comparison = search_comparison,
    validation_probability_comparison = ">=",
    status = status, reference = "10.1037/met0000555"
  )
  report <- paste0("Native powerly::validate independent simulations at N = ", summary$sample_size,
    ": ", successes, "/", length(measures), " reached the performance threshold ", target_value,
    "; target-achievement probability = ", signif(uncertainty$probability, 3),
    ", exact-binomial 95% interval [", signif(uncertainty$lower, 3), ", ", signif(uncertainty$upper, 3),
    "] (target probability ", target_probability, "). Validation status: ", status,
    ". Results remain conditional on the assumed network and native data-generation/estimation settings.")
  if (!identical(search_comparison, ">=")) report <- paste(report,
    "The native decreasing search uses a <= probability crossing; native validation still evaluates >= target attainment. These are different native criteria.")
  structure(list(method = "powerly", model = plan$model, fit = fit, results = fit$measures,
    true_network = plan$true_network, settings = settings, summary = summary,
    status = status, report = report), class = "quicknet_power_validation")
}

#' @export
print.quicknet_power_validation <- function(x, ...) {
  cat("<quicknet_power_validation>\n", x$report, "\n", sep = "")
  invisible(x)
}

#' @export
summary.quicknet_power_validation <- function(object, ...) {
  object$summary
}

#' Plot native powerly validation results
#' @param x A quicknet_power_validation object.
#' @param ... Arguments forwarded to powerly's native validation plot method.
#' @return The native plot result.
#' @export
plot.quicknet_power_validation <- function(x, ...) {
  plot(x$fit, ...)
}
