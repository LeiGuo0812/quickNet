#' Calculate the correlation between two networks
#' @description This function computes Mantel's permutation test for similarity of two matrices. It permutes the rows and columns of the second matrix randomly and calculates a Z-statistic.
#' @importFrom ape mantel.test
#' @param x1 the first network, should be the product of \code{quickNet::quickNet()}, or an adjacency matrix of network data.
#' @param x2 the second network, should be the product of \code{quickNet::quickNet()}, or an adjacency matrix of network data.
#' @param nperm the number of times to permute the data.
#' @param graph a logical indicating whether to produce a summary graph.
#' @param alternative a character string defining the alternative hypothesis: "two.sided" (default), "less", "greater", or any unambiguous abbreviation of these.
#' @param ... further arguments to be passed to plot() (to add a title, change the axis labels, and so on).
#' @details The function calculates a Z-statistic for the Mantel test, equal to the sum of the pairwise product of the lower triangles of the permuted matrices, for each permutation of rows and columns. It compares the permuted distribution with the Z-statistic observed for the actual data.
#' Node labels must be exchangeable for this permutation test. It tests alignment
#' of network nodes; it does not resample participants or test equality of two
#' population networks. Only undirected symmetric networks are supported.
#' @return \itemize{
#' \item \code{z.stat}: the Z-statistic (sum of row-column products in the
#' lower triangle) of the data matrices.
#' \item \code{p}: P-value (quantile of the observed Z-statistic in the
#' permutation distribution).
#' \item \code{alternative}: the alternative hypothesis.
#' }
#' @export
#'
#' @examples
#' data('mtcars')
#' net1 <- quickNet(mtcars)
#' net2 <- quickNet(mtcars^3)
#' netCor(net1, net2)
#'
netCor <- function(x1, x2, nperm = 999, graph = FALSE, alternative = 'two.sided',...){

  if (!quicknet_is_positive_integer(nperm)) stop("nperm must be a positive integer.", call. = FALSE)
  if (!is.logical(graph) || length(graph) != 1L || is.na(graph)) stop("graph must be TRUE or FALSE.", call. = FALSE)
  matrix1 <- quicknet_network_matrix(x1)
  matrix2 <- quicknet_align_network(matrix1, quicknet_network_matrix(x2))

  if (!isSymmetric(unname(matrix1)) || !isSymmetric(unname(matrix2)) ||
      quicknet_is_directed(x1) ||
      quicknet_is_directed(x2)) {
    stop("netCor requires undirected, symmetric networks; ape's Mantel test uses only one triangle.", call. = FALSE)
  }
  return(ape::mantel.test(matrix1, matrix2, nperm = nperm, graph = graph, alternative = alternative, ...))

}
