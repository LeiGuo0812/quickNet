#' Calculate coupling value with Multiplication of Temporal Derivatives method.
#'
#' @param data A matrix or a data frame. The rows should be observations or time points. The number of columns should be two.
#' @param nperm The number of permutations to test the significant of coupling.
#' @details This function apply the method of Multiplication of Temporal Derivatives introduced by Shine, et al (2015). But note that this function doesn't apply time series smooth.
#' @references Shine, J. M., Koyejo, O., Bell, P. T., Gorgolewski, K. J., Gilat, M., & Poldrack, R. A. (2015). Estimation of dynamic functional connectivity using Multiplication of Temporal Derivatives. NeuroImage, 122, 399–407. https://doi.org/10.1016/j.neuroimage.2015.07.064
#'
#' @return A list. \itemize{
#' \item{\code{coupling}: The coupling of the two columns in each observation.}
#' \item{\code{coupling_mean}: The mean coupling value of all the observations, representing the overall coupling between the two columns.}
#' \item{\code{p.value}: The p value of the coupling value from permutation test.}
#' }
#'
#' @export
#'
#' @examples
#' MTD.No.Smooth.Test(mtcars[,1:2])
#'
MTD.No.Smooth.Test <- function(data, nperm = 5000){

  MTD.No.Smooth <- function(x){

    diff <- apply(x, 2, diff)
    sd <- apply(x,2,sd) %>%
      `dim<-`(c(1,2))

    diff_std <- array(dim = c(nrow(diff), ncol(diff)))

    for (i in seq_len(ncol(diff))) {
      diff_std[,i] <- diff[,i]/sd[,i]
    }

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

  p.value <- mean(abs(perm_coupling) >= origin_coupling$coupling_mean)

  MTD <- origin_coupling

  MTD$p.value <- p.value

  return(MTD)
}


