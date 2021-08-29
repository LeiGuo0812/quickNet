#' @title Gaussian Markov random field estimation using graphical LASSO and extended Bayesian information criterion
#' @description short version of botnet::estimateNetwork(data, default = 'EBICglasso')
#' @param data a data frame, each column presents a node, there should be no miss values in the data frame.
#' @param ... paramaters from \code{bootnet::estimateNetwork}
#' @export
#' @examples
#' data('mtcars')
#' EBICglassoNet(mtcars)
#'
EBICglassoNet <- function(data, ...){
  bootnet::estimateNetwork(data, default = 'EBICglasso', ...)
}
