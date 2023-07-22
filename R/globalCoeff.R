#' Compute global coefficients of a net work.
#' @importFrom NetworkToolbox pathlengths clustcoeff
#' @param x a network produced from \code{quickNet}, or an adjacency matrix of network data.
#' @param list whether arrange the results as a list. The default is FALSE.
#'
#' @return \itemize{
#' \item{\code{globalStrength:} the global strength.}
#' \item{\code{AGS:} the average of all the edge weights in the network. equals globalStrength/(N * (N-1)/2), where N is the number of nodes in the network.}
#' \item{\code{ASPL:} the global average shortest path length.}
#' \item{\code{CC:} the global clustering coefficient. The average clustering coefficient for each node in the network.}
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

  if (!is.logical(list)) {
    stop('list should be logical.')
  }

  if (sum(class(x) == 'qgraph') > 0) {
    network <- x$graphData$graph
  } else if (is.matrix(x)) {
    network <- x
  }

  globalStrength <- sum(abs(network[upper.tri(network)]))

  nNodes <- dim(network)[1]

  AGS <- globalStrength / (nNodes * (nNodes - 1) / 2)
  ASPL <- pathlengths(network)[[1]]
  CC <- clustcoeff(network)[[1]]

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
