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
#' @return \itemize{
#' \item{\code{z.stat:} 	the Z-statistic (sum of rows*columns of lower triangle) of the data matrices.}
#' \item{\code{p:} P-value (quantile of the observed Z-statistic in the permutation distribution).}
#' \item{\code{alternative:} the alternative hypothesis.}
#' }
#' @export
#'
#' @examples
#' data('mtcars')
#' net1 <- quickNet(mtcars)
#' net2 <- quickNet(mtcars^3)
#' netCor(net1, net2)
#'
netCor <- function(x1, x2, nperm = 1000, graph = TRUE, alternative = 'two.sided',...){

  if (sum(class(x1) == 'qgraph') > 0) {
    matrix1 <- x1$graphData$graph
  } else if (is.matrix(x1)) {
    matrix1 <- x1
  }

  if (sum(class(x2) == 'qgraph') > 0) {
    matrix2 <- x2$graphData$graph
  } else if (is.matrix(x2)) {
    matrix2 <- x2
  }

  return(ape::mantel.test(matrix1, matrix2, nperm = nperm, graph = graph, alternative = alternative, ...))

}
