#' Calculate coupling with the Multiplication of Temporal Derivatives method
#'
#' @param data A matrix or a data frame. The rows should be observations or time points. The number of columns should be two.
#' @param nperm The number of permutations to test the significant of coupling.
#' @details This function apply the method of Multiplication of Temporal Derivatives introduced by Shine, et al (2015). But note that this function doesn't apply time series smooth.
#' @references Shine, J. M., Koyejo, O., Bell, P. T., Gorgolewski, K. J., Gilat, M., & Poldrack, R. A. (2015). Estimation of dynamic functional connectivity using Multiplication of Temporal Derivatives. NeuroImage, 122, 399-407. https://doi.org/10.1016/j.neuroimage.2015.07.064
#'
#' @return A list. \itemize{
#' \item \code{coupling}: The coupling of the two columns in each observation.
#' \item \code{coupling_mean}: The mean coupling value of all the observations,
#' representing the overall coupling between the two columns.
#' \item \code{p.value}: The permutation-test p value of the coupling.
#' }
#'
#' @export
#'
#' @examples
#' MTD.No.Smooth.Test(mtcars[,1:2])
#'
MTD.No.Smooth.Test <- function(data, nperm = 5000){
  data <- as.matrix(data)
  if (!is.numeric(data) || ncol(data) != 2 || nrow(data) < 3) {
    stop("data must contain exactly two numeric columns and at least three rows.", call. = FALSE)
  }
  if (any(!is.finite(data))) {
    stop("data must not contain missing or non-finite values.", call. = FALSE)
  }
  if (!quicknet_is_positive_integer(nperm)) {
    stop("nperm must be a positive integer.", call. = FALSE)
  }

  MTD.No.Smooth <- function(x){

    derivatives <- apply(x, 2, base::diff)
    standard_deviations <- apply(x, 2, stats::sd)
    if (any(!is.finite(standard_deviations)) || any(standard_deviations == 0)) {
      stop("Both data columns must have nonzero finite standard deviations.", call. = FALSE)
    }

    diff_std <- sweep(derivatives, 2, standard_deviations, "/")

    coupling <- array(dim = c(nrow(diff_std), ncol(diff_std), ncol(diff_std)))

    for (k in seq_len(nrow(diff_std))) {
      for(i in seq_len(ncol(diff_std))) {
        coupling[k,i,] <- diff_std[k,i] * diff_std[k,]
      }
    }

    results <- list()
    results$coupling <- coupling
    results$coupling_mean <- apply(coupling, c(2,3), mean)[1,2]
    return(results)
  }

  origin_coupling <- MTD.No.Smooth(data)

  perm_coupling <- rep(NA, nperm)

  for (i in seq_len(nperm)) {
    data_new <- cbind(data[,1], sample(data[,2]))
    perm_coupling[i] <-MTD.No.Smooth(data_new)$coupling_mean
  }

  p.value <- (
    sum(abs(perm_coupling) >= abs(origin_coupling$coupling_mean)) + 1
  ) / (nperm + 1)

  MTD <- origin_coupling

  MTD$p.value <- p.value

  return(MTD)
}


