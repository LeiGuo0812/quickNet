#' @title estimate the edge weight and node stability of a network.
#' @importFrom bootnet bootnet corStability
#' @param data a data frame, each column presents a node, there should be no miss values in the data frame.
#' @param nboot number of bootstraps.
#' @param ncore number of cores to use in computing results. Set to 1 to not use parallel computing.
#' @param labels use self-specified node labels, typically the \code{labels} parameter you put in the \code{quickNet} function.
#' @param add.bridge a logical value to determine whether to calculate bridge coefficients or not. If the value is TRUE, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness" will be added to the results.
#' @param communities used for bridge centrality measures. If add.bridge is set TRUE, this should be provided. see \code{netowrktools::bridge}.
#' @param useCommunities character vector specifying which communities should be included. Default set to "all".
#' @param cor When calculating Correlation stability coefficient, (CS-coefficient), the correlation level to test at. Default is 0.7.
#' @return a list contains the stability test results of the netowrk\itemize{
#' \item\code{boot_edge_weight_stabilty:} the bootstrap result of edge weight accuracy.
#' \item\code{boot_centrality_stabilty:} the bootstrap result of centrality stability.
#' \item\code{edge_weight_CI_plot:} the plot of edge weight CI.
#' \item\code{edge_weight_diff_plot:} the plot of pair-wise edge difference.
#' \item\code{centrality_stability_plot:} the plot of node centrality stability.
#' \item\code{centrality_diff_plot:} the plot of pair-wise node centrality difference.
#' \item\code{CS_coefficient:} the Centrality stability coefficient (CS-coefficient) of all statistics.
#' }
#' @export
#'
#' @examples
#'
#' data('mtcars')
#' Stability <- Stability(mtcars, nboot = 100)
#'
#' Stability2 <- Stability(mtcars, nboot = 100, add.bridge = TRUE, communities = list(c1 = 1:5, c2 = 6:11))
#'

Stability <- function(data, nboot = 1000, ncore = 1, labels = NULL,  add.bridge = FALSE, communities = NULL, useCommunities = 'all', cor = 0.7){

  if (!is.logical(add.bridge)) {
    stop('Error: add.bridge should be logical.')
  }

  if (!is.null(labels)) {
    colnames(data) <- labels
  }

  network <- EBICglassoNet(data)

  results <- list()

  statistics <- c("edge", "strength", "closeness", "betweenness", "length", "distance", "expectedInfluence")

  if (add.bridge) {
    statistics <- c(statistics, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness")
  }

  boota <- bootnet(network, nBoots = nboot, nCores = ncore)
  bootb <- bootnet(network, nBoots = nboot, type = "case",  nCores = ncore, statistics = statistics, communities = communities, useCommunities = useCommunities)

  results$boot_edge_weight_stabilty <- boota

  results$boot_centrality_stabilty <- bootb

  results$edge_weight_CI_plot <- plot(boota, labels = FALSE, order = "sample")

  try(results$edge_weight_diff_plot <- plot(boota, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample"))

  results$centrality_stability_plot <- plot(bootb,statistics=c('strength','closeness','betweenness'))

  results$centrality_diff_plot <- plot(boota, "strength", order="sample", labels=TRUE)

  if (add.bridge) {
    results$brige_stability_plot <- plot(bootb,statistics=c("bridgeStrength", "bridgeCloseness", "bridgeBetweenness"))
  }

  results$CS_coefficient <- corStability(bootb, cor = cor)

  return(results)
}
