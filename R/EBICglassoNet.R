#' @title Gaussian Markov random field estimation using graphical LASSO and extended Bayesian information criterion
#' @description Estimate an EBICglasso network and return a quicknet_fit object.
#' @param data A data frame with one numeric column per node; missing-data handling follows the selected estimator.
#' @param gamma EBIC hyperparameter in [0,1]. NULL selects 0.5.
#' @param ... Named estimation arguments passed directly to the bootnet
#'   EBICglasso estimator, e.g. \code{nlambda = 50}, \code{missing = "listwise"}.
#'   Source defaults are used for omitted controls.
#' @export
#' @examples
#' data('mtcars')
#' EBICglassoNet(mtcars)
#'
EBICglassoNet <- function(data, gamma = NULL, ...){
  quicknet_fit_cross_sectional(data, model = "EBICglasso", gamma = gamma, backend_args = list(...))
}
