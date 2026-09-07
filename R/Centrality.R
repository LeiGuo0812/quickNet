#' @title estimate the node Strength, Betweenness and Closeness of a network
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom qgraph centralityPlot centrality
#' @param network_G a qgraph object, could also be the result of quickNet.
#' @param include a vector of measures to include. if missing all measures available will be included. Not included by default are "Closeness", "Betweenness", "ExpectedInfluence", "OutExpectedInfluence", and "InExpectedInfluence". Can also be "all" or "All" to include all available centrality measures.
#' @param ... other parameters from \code{qgraph::centralityPlot}, this only affect the output of Centrality$centralityPlot.
#' @return a list contains the centrality information:\itemize{
#' \item\code{centralityPlot:} the result of \code{qgraph::centralityPlot}.
#' \item\code{centrality_data:} the result of \code{qgraph::centrality}.}
#' @export
#' @examples
#' data('mtcars')
#' Centrality <- Centrality(quickNet(mtcars))

Centrality <- function(network_G, include = 'all', ...){

  results <- list()

  network_matrix <- quicknet_network_matrix(network_G)
  cp_input <- if (inherits(network_G, "quicknet_fit") && !is.null(network_G$plots$network)) {
    network_G$plots$network
  } else if (quicknet_is_directed(network_G)) {
    qgraph::qgraph(
      quicknet_to_qgraph_matrix(network_matrix, directed = TRUE),
      directed = TRUE,
      DoNotPlot = TRUE
    )
  } else {
    network_matrix
  }

  plot_args <- list(...)
  if (is.null(plot_args$scale) && is.null(plot_args$standardized) && is.null(plot_args$relative)) {
    plot_args$scale <- "raw"
  }
  cp <- do.call(centralityPlot, c(list(cp_input, include = include), plot_args))

  cp_data <- centrality(cp_input)

  cp_data_scale <- cp_data[1:6] %>%
    map(~ quicknet_standardize_centrality(.x) %>%
          `names<-`(names(cp_data$OutDegree)))
  names(cp_data_scale) <- paste0(names(cp_data_scale), 'Scale')

  cp_data_all <- c(cp_data, cp_data_scale)

  results$centralityPlot <- cp

  results$centrality_data <- cp_data_all
  results$node_table <- if (quicknet_is_directed(network_G)) {
    quicknet_directed_node_table(network_matrix)
  } else {
    quicknet_node_table(network_matrix)
  }

  return(results)
}

quicknet_standardize_centrality <- function(x) {
  out <- rep(NA_real_, length(x))
  valid <- is.finite(x)
  if (any(valid)) {
    values <- x[valid]
    out[valid] <- if (length(values) < 2L || stats::sd(values) == 0) 0 else as.numeric(scale(values))
  }
  out
}
