#' @title Estimate edge-weight and node stability of a network
#' @importFrom bootnet bootnet corStability
#' @param data a data frame or \code{quicknet_fit} object.
#' @param nboot number of bootstraps.
#' @param ncore number of cores to use in computing results. Set to 1 to not use parallel computing.
#' @param labels use self-specified node labels, typically the \code{labels} parameter you put in the \code{quickNet} function.
#' @param model network model used when \code{data} is a data frame.
#' @param cor_method correlation method used by correlation and partial models.
#' @param missing missing-data handling.
#' @param gamma EBIC tuning parameter.
#' @param ordinal_method association method used by ordinal models.
#' @param AND logical. Should the Ising model use the AND rule?
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param case.drop proportions used for case-dropping centrality stability.
#' @param add.bridge a logical value to determine whether to calculate bridge coefficients or not. If the value is TRUE, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness" will be added to the results.
#' @param communities used for bridge centrality measures. If add.bridge is set TRUE, this should be provided. See \code{networktools::bridge}.
#' @param useCommunities character vector specifying which communities should be included. Default set to "all".
#' @param cor When calculating Correlation stability coefficient, (CS-coefficient), the correlation level to test at. Default is 0.7.
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

Stability <- function(data, nboot = 1000, ncore = 1, labels = NULL, model = "EBICglasso", cor_method = "pearson", missing = "listwise", gamma = 0.5, ordinal_method = "polychoric", AND = TRUE, types = NULL, levels = NULL, case.drop = c(0.10, 0.25, 0.50), add.bridge = FALSE, communities = NULL, useCommunities = 'all', cor = 0.7){

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
    network <- data
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
      levels = levels
    )
  }

  results <- list()
  results$fit <- network
  results$edge_bootstrap_stability <- quicknet_bootstrap_edge_stability(network, nboot = nboot)
  results$case_drop_centrality_stability <- quicknet_case_drop_centrality_stability(
    network,
    nboot = nboot,
    proportions = case.drop
  )

  statistics <- c("edge", "strength", "closeness", "betweenness", "length", "distance", "expectedInfluence")

  if (add.bridge) {
    statistics <- c(statistics, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness")
  }

  if (network$model != "EBICglasso") {
    return(results)
  }

  boota <- bootnet(network$fit, nBoots = nboot, nCores = ncore)
  bootb <- bootnet(network$fit, nBoots = nboot, type = "case",  nCores = ncore, statistics = statistics, communities = communities, useCommunities = useCommunities)

  results$boot_edge_weight_stability <- boota

  results$boot_centrality_stability <- bootb

  results$edge_weight_CI_plot <- plot(boota, labels = FALSE, order = "sample")

  try(results$edge_weight_diff_plot <- plot(boota, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample"))

  results$centrality_stability_plot <- plot(bootb,statistics=c('strength','closeness','betweenness'))

  results$centrality_diff_plot <- plot(boota, "strength", order="sample", labels=TRUE)

  if (add.bridge) {
    results$bridge_stability_plot <- plot(bootb,statistics=c("bridgeStrength", "bridgeCloseness", "bridgeBetweenness"))
  }

  results$CS_coefficient <- corStability(bootb, cor = cor)

  return(results)
}
