#' @title Estimate edge-weight and node stability of a network
#' @importFrom bootnet bootnet corStability
#' @param data a data frame or \code{quicknet_fit} object.
#' @param nboot number of bootstraps.
#' @param ncore number of cores to use in computing results. Set to 1 to not use parallel computing.
#' @param labels use self-specified node labels, typically the \code{labels} parameter you put in the \code{quickNet} function.
#' @param model network model used when \code{data} is a data frame.
#' @param cor_method Correlation method; NULL inherits the selected estimator.
#' @param missing Missing-data rule; NULL inherits the selected estimator.
#' @param gamma EBIC hyperparameter in [0,1]. NULL uses model-specific
#'   defaults for raw data (0.5 for EBICglasso, 0.25 for Ising or EBIC-selected
#'   MGM). MGM defaults to CV, with inactive gamma. For a fitted object its
#'   original gamma is retained; conflicting overrides
#'   are rejected. Gamma is not used by non-EBIC models.
#' @param ordinal_method association method used by ordinal models.
#' @param AND logical. Should the Ising model use the AND rule?
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param case.drop proportions used for case-dropping centrality stability.
#' @param add.bridge a logical value to determine whether to calculate bridge coefficients or not. If the value is TRUE, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness" will be added to the results.
#' @param communities used for bridge centrality measures. If add.bridge is set TRUE, this should be provided. See \code{networktools::bridge}.
#' @param useCommunities character vector specifying which communities should be included. Default set to "all".
#' @param cor When calculating Correlation stability coefficient, (CS-coefficient), the correlation level to test at. Default is 0.7.
#' @param ... Named model arguments for raw data, e.g. \code{lambdaSel = "EBIC"}.
#'   Fitted inputs retain their original settings. Observation-specific
#'   arguments such as weights cannot be automatically realigned during resampling.
#' @details The custom edge table uses observation bootstrap percentile intervals
#'   (2.5th and 97.5th percentiles), conditional on successful fits. Failed fits
#'   are not replaced; requested, successful, failed and undefined counts are
#'   reported. Failure causes are stored in \code{resampling} and the table's
#'   \code{resampling} attribute.
#'   The custom case-drop table summarizes correlations of centrality vectors
#'   at each requested deletion proportion and records the actual sample size.
#'   A constant centrality vector gives an undefined correlation, not a failed fit.
#'   This table is not the bootnet CS coefficient. For EBICglasso, the additional
#'   bootnet results and \code{CS_coefficient} use bootnet's own sampling and
#'   retry rules; CS is computed by \code{bootnet::corStability()}.
#' @return a list contains the stability test results of the network\itemize{
#' \item\code{boot_edge_weight_stability:} the bootstrap result of edge weight accuracy.
#' \item\code{boot_centrality_stability:} the bootstrap result of centrality stability.
#' \item\code{edge_weight_CI_plot:} the plot of edge weight CI.
#' \item\code{edge_weight_diff_plot:} the plot of pair-wise edge difference.
#' \item\code{centrality_stability_plot:} the plot of node centrality stability.
#' \item\code{centrality_diff_plot:} the plot of pair-wise node centrality difference.
#' \item\code{bridge_stability_plot:} the plot of bridge stability when \code{add.bridge = TRUE}.
#' \item\code{CS_coefficient:} the Centrality stability coefficient (CS-coefficient) of all statistics.
#' \item\code{edge_bootstrap_stability:} model-agnostic row bootstrap edge stability table.
#' \item\code{case_drop_centrality_stability:} model-agnostic case-dropping centrality stability table.
#' }
#' @export
#'
#' @examples
#'
#' data('mtcars')
#' Stability <- Stability(mtcars, nboot = 10)
#'
#' Stability2 <- Stability(
#'   mtcars,
#'   nboot = 10,
#'   add.bridge = TRUE,
#'   communities = list(c1 = 1:5, c2 = 6:11)
#' )
#'

Stability <- function(data, nboot = 1000, ncore = 1, labels = NULL, model = "EBICglasso", cor_method = NULL, missing = NULL, gamma = NULL, ordinal_method = "polychoric", AND = TRUE, types = NULL, levels = NULL, case.drop = c(0.10, 0.25, 0.50), add.bridge = FALSE, communities = NULL, useCommunities = 'all', cor = 0.7, ...){

  backend_args <- list(...)
  if (!quicknet_is_positive_integer(nboot)) {
    stop("nboot must be a positive integer.", call. = FALSE)
  }
  if (!quicknet_is_positive_integer(ncore)) {
    stop("ncore must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(case.drop) || length(case.drop) == 0 ||
      any(!is.finite(case.drop)) || any(case.drop <= 0 | case.drop >= 1)) {
    stop("case.drop must contain finite proportions in (0, 1).", call. = FALSE)
  }
  if (!is.logical(add.bridge) || length(add.bridge) != 1 || is.na(add.bridge)) {
    stop('Error: add.bridge should be logical.')
  }

  if (inherits(data, "quicknet_fit")) {
    if (!is.null(gamma) && !identical(
      quicknet_resolve_gamma(data$model, gamma), quicknet_fit_gamma(data)
    )) {
      stop("gamma cannot override a fitted object's setting; refit the model first.", call. = FALSE)
    }
    supplied <- as.list(match.call())[-1L]
    checked <- intersect(names(supplied), c("model", "cor_method", "missing", "ordinal_method", "AND", "types", "levels"))
    settings <- quicknet_cross_refit_args(data)
    for (name in checked) {
      if (!isTRUE(all.equal(get(name), settings[[name]], check.attributes = FALSE))) {
        stop(name, " cannot override a fitted object's setting; refit the model first.", call. = FALSE)
      }
    }
    if (length(backend_args) && !isTRUE(all.equal(backend_args, settings$backend_args))) stop("Backend arguments cannot override a fitted object during stability analysis.", call. = FALSE)
    if (!is.null(labels)) stop("labels cannot rename a fitted object during stability analysis.", call. = FALSE)
    network <- data
    network$meta$gamma <- quicknet_refit_gamma(network)
  } else {
    if (!is.null(labels)) {
      colnames(data) <- labels
    }
    network <- quicknet_fit_cross_sectional(
      data = data,
      model = model,
      cor_method = cor_method,
      missing = missing,
      gamma = gamma,
      ordinal_method = ordinal_method,
      AND = AND,
      types = types,
      levels = levels,
      backend_args = backend_args
    )
  }

  quicknet_check_row_args(quicknet_cross_refit_args(network)$backend_args, "Stability")
  if (network$model != "EBICglasso" && (ncore != 1 || add.bridge || !missing(cor))) {
    stop("ncore, add.bridge and cor are bootnet controls supported only for EBICglasso fits.", call. = FALSE)
  }
  failure_reason <- quicknet_fit_failure_reason(network)
  if (!is.null(failure_reason)) stop("The original fit is not valid: ", failure_reason, call. = FALSE)
  results <- list()
  results$fit <- network
  results$edge_bootstrap_stability <- quicknet_bootstrap_edge_stability(network, nboot = nboot)
  results$case_drop_centrality_stability <- quicknet_case_drop_centrality_stability(
    network,
    nboot = nboot,
    proportions = case.drop
  )

  results$resampling <- list(
    edges = attr(results$edge_bootstrap_stability, "resampling"),
    case_drop = attr(results$case_drop_centrality_stability, "resampling")
  )

  statistics <- c("edge", "strength", "closeness", "betweenness", "length", "distance", "expectedInfluence")

  if (add.bridge) {
    statistics <- c(statistics, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness")
  }

  if (network$model != "EBICglasso") {
    return(results)
  }

  boota <- quicknet_bootnet_resampling(network$fit, nBoots = nboot, nCores = ncore)
  bootb <- quicknet_bootnet_resampling(network$fit, nBoots = nboot, type = "case",  nCores = ncore, statistics = statistics, communities = communities, useCommunities = useCommunities)

  results$resampling$bootnet_edges <- attr(boota, "resampling")
  results$resampling$bootnet_case <- attr(bootb, "resampling")

  results$boot_edge_weight_stability <- boota

  results$boot_centrality_stability <- bootb

  results$edge_weight_CI_plot <- plot(boota, labels = FALSE, order = "sample")

  try(results$edge_weight_diff_plot <- plot(boota, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample"))

  results$centrality_stability_plot <- plot(bootb,statistics=c('strength','closeness','betweenness'))

  results$centrality_diff_plot <- plot(boota, "strength", plot="difference", order="sample", labels=TRUE)

  if (add.bridge) {
    results$bridge_stability_plot <- plot(bootb,statistics=c("bridgeStrength", "bridgeCloseness", "bridgeBetweenness"))
  }

  results$CS_coefficient <- corStability(bootb, cor = cor)

  return(results)
}

# Native bootnet owns its retry policy. Preserve its warning evidence without
# interpreting unreported failures as zero or replacing its estimator/sampling.
quicknet_bootnet_resampling <- function(data, nBoots, ...) {
  warnings <- character()
  result <- withCallingHandlers(bootnet(data, nBoots = nBoots, ...), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
  })
  pattern <- "^[0-9]+ bootstrap estimation\\(s\\) failed and were resampled[.]$"
  counts <- warnings[grepl(pattern, warnings)]
  failures <- if (length(counts)) sum(as.integer(sub(" .*", "", counts))) else NA_integer_
  attr(result, "resampling") <- list(method = "bootnet_native", requested = nBoots,
    returned = length(result$boots), reported_failed_attempts = failures,
    failure_policy = "Native bootnet retries failed fits with newly sampled data; unreported counts remain unknown.",
    warnings = unique(warnings), backend_version = as.character(utils::packageVersion("bootnet")))
  result
}
