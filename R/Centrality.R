#' @title estimate the node Strength, Betweenness and Closeness of a network
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom qgraph centralityPlot centrality
#' @param network_G a qgraph object, could also be the result of quickNet.
#' @param include a vector of measures to include. if missing all measures available will be included. Not included by default are "Closeness", "Betweenness", "ExpectedInfluence", "OutExpectedInfluence", and "InExpectedInfluence". Can also be "all" or "All" to include all available centrality measures.
#' @param ... other parameters from \code{qgraph::centralityPlot}, this only affect the output of Centrality$centralityPlot.
#' @return a list contains the centrality information:\itemize{
#' \item\code{centralityPlot:} the result of \code{qgraph::centralityPlot}.
#' \item\code{centrailty_data:} the result of \code{qgraph::centrality}.}
#' @export
#' @examples
#' data('mtcars')
#' Centrality <- Centrality(quickNet(mtcars))

Centrality <- function(network_G, include = 'all', ...){

  results <- list()

  cp <- centralityPlot(network_G, include = include, ...)

  cp_data <- centrality(network_G)

  cp_data_scale <- cp_data[1:6] %>%
    map(~ as.numeric(scale(.x)) %>%
          `names<-`(names(cp_data$OutDegree)))
  names(cp_data_scale) <- paste0(names(cp_data_scale), 'Scale')

  cp_data_all <- c(cp_data, cp_data_scale)

  results$centralityPlot <- cp

  results$centrality_data <- cp_data_all

  return(results)
}
