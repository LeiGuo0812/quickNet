#' @title Gaussian Markov random field estimation using graphical LASSO and extended Bayesian information criterion
#' @description Estimate an EBICglasso network and return a quicknet_fit object.
#' @param data a data frame, each column presents a node, there should be no miss values in the data frame.
#' @param gamma EBIC tuning parameter.
#' @param ... Reserved for future extensions.
#' @export
#' @examples
#' data('mtcars')
#' EBICglassoNet(mtcars)
#'
EBICglassoNet <- function(data, gamma = 0.5, ...){
  quicknet_fit_cross_sectional(data, model = "EBICglasso", gamma = gamma)
}
