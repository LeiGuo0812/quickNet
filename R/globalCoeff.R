#' Compute global coefficients of a network
#' @importFrom NetworkToolbox pathlengths clustcoeff
#' @param x a network produced from \code{quickNet}, or an adjacency matrix of network data.
#' @param list whether arrange the results as a list. The default is FALSE.
#'
#' @return \itemize{
#' \item \code{globalStrength}: the global strength.
#' \item \code{AGS}: the average of all the edge weights in the network. It
#' equals globalStrength/(N * (N-1)/2), where N is the number of nodes.
#' \item \code{ASPL}: the global average shortest path length.
#' \item \code{CC}: the global clustering coefficient, averaged across nodes.
#' }
#' @export
#'
#' @examples
#'
#' data('mtcars')
#'
#' globalCoeff <- globalCoeff(quickNet(mtcars))
#'
globalCoeff <- function(x, list = FALSE){

  if (!is.logical(list) || length(list) != 1 || is.na(list)) {
    stop('list should be logical.')
  }

  network <- quicknet_network_matrix(x)
  diag(network) <- 0
  directed <- quicknet_is_directed(x)

  edge_values <- if (directed) network[row(network) != col(network)] else network[upper.tri(network)]
  globalStrength <- sum(abs(edge_values), na.rm = TRUE)

  AGS <- if (length(edge_values) > 0) globalStrength / length(edge_values) else 0
  ASPL <- NetworkToolbox::pathlengths(network)[[1]]
  CC <- NetworkToolbox::clustcoeff(network)[[1]]

  if (list == FALSE) {
    result <- data.frame(
      globalStrength = globalStrength,
      AGS = AGS,
      ASPL = ASPL,
      CC = CC
    )
  } else {
    result <- list(
      globalStrength = globalStrength,
      AGS = AGS,
      ASPL = ASPL,
      CC = CC
    )
  }
  return(result)
}
