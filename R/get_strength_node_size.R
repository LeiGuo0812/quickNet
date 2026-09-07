#' Get new node size for graph based on strength centrality
#'
#' @importFrom scales rescale
#'
#' @param Centrality The output of function \code{Centrality}
#' @param scale_add The basic size added to original size, in case of the node is too small
#' @param scale_times The scale times of the original size in case the node is too small
#'
#' @return a named vector containing the new sizes for each node.
#' @export
#'
#' @examples
#' data("mtcars")
#' network = quickNet(mtcars)
#' centrality = Centrality(network)
#' vsize = get_strength_node_size(centrality)
#'
#' quickNet(mtcars, vsize = vsize)
get_strength_node_size = function(Centrality, scale_add = 0.5, scale_times = 2.5) {
  strength = Centrality$centrality_data$InDegreeScale
  if (is.null(strength) || !is.numeric(strength)) {
    stop("Centrality must contain numeric InDegreeScale values.", call. = FALSE)
  }
  valid <- is.finite(strength)
  sizes <- rep(scale_add * scale_times, length(strength))
  if (any(valid)) {
    sizes[valid] <- (rescale(strength[valid],
      to = range(abs(strength[valid]))) + scale_add) * scale_times
  }
  names(sizes) <- names(strength)
  sizes
}
