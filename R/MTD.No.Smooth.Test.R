#' Calculate coupling with the Multiplication of Temporal Derivatives method
#'
#' @param data Two numeric columns of equally spaced observations from one pair
#'   of time series, in temporal order. Missing values are not allowed.
#' @param nperm Number of random permutations for \code{method = "shuffle"}.
#'   TTS uses every allowed shift; omit \code{nperm} when using TTS.
#' @param method Significance test. \code{"tts"} uses the truncated time-shift
#'   procedure of Yuan and Shou (2024). \code{"shuffle"} permutes the second
#'   column's raw observations and requires their exchangeability under the null.
#' @param radius Required truncation radius for TTS, chosen before examining
#'   test results. A nonnegative integer with \code{2 * radius < nrow(data) - 1}.
#'   The authors' reference implementation has no default radius.
#' @details Coupling follows Shine et al. (2015), without temporal smoothing.
#'   First differences are divided by their full-series sample standard
#'   deviations, as in the authors' MATLAB implementation.
#'
#'   TTS tests independence of the first-difference processes. The first
#'   differences of the second column must be strictly stationary under the
#'   null; stationarity is an assumption, not established by this function.
#'   With \code{m = nrow(data) - 1} and radius \code{r}, the first derivative
#'   series is restricted to indices \code{(r + 1):(m - r)}. The second is
#'   shifted by every integer from \code{-r} to \code{r}, without wrapping.
#'   The statistic is the absolute mean product. The common full-series
#'   standardization cancels in the ordering of shifted statistics.
#'
#'   If \code{B} statistics (including zero shift and ties) are at least as
#'   large as the zero-shift statistic, TTS returns
#'   \code{p.value = min(1, B / (r + 1))}. No additional plus-one correction
#'   is applied. The smallest possible p value is \code{1 / (r + 1)};
#'   short series or small radii therefore limit significance resolution.
#'   This test concerns the retained central window. \code{coupling_mean}
#'   remains the descriptive mean over all first differences;
#'   \code{test_coupling_mean} is the signed mean actually tested.
#'
#'   The explicit \code{"shuffle"} option retains a two-sided random
#'   permutation test with the plus-one correction. Arbitrary time shuffling
#'   is not valid for general autocorrelated series. Neither test establishes
#'   a causal direction or rules out dependence when it is not significant.
#' @references Shine, J. M., Koyejo, O., Bell, P. T., Gorgolewski, K. J.,
#'   Gilat, M., & Poldrack, R. A. (2015). Estimation of dynamic functional
#'   connectivity using Multiplication of Temporal Derivatives.
#'   NeuroImage, 122, 399-407. doi:10.1016/j.neuroimage.2015.07.064.
#'
#'   Yuan, A. E., & Shou, W. (2024). A rigorous and versatile statistical test
#'   for correlations between stationary time series.
#'   PLoS Biology, 22(8), e3002758. doi:10.1371/journal.pbio.3002758.
#' @return A list containing \code{coupling} (time by 2 by 2 array),
#'   \code{coupling_mean}, \code{p.value}, \code{method},
#'   \code{test_coupling_mean}, \code{statistic} (its absolute value),
#'   \code{null_coupling} (signed resampled means), \code{extreme_count},
#'   \code{minimum_p}, \code{derivative_indices} (the tested first differences),
#'   and \code{assumption}. TTS also returns \code{radius}, \code{shifts}
#'   and \code{tts_bound} (the unclipped bound); shuffle returns \code{nperm}.
#' @export
#' @examples
#' set.seed(1)
#' series <- cbind(as.numeric(arima.sim(list(ar = 0.5), n = 160)),
#'                 as.numeric(arima.sim(list(ar = 0.5), n = 160)))
#' MTD.No.Smooth.Test(series, radius = 39)$p.value
MTD.No.Smooth.Test <- function(data, nperm = 5000,
                              method = c("tts", "shuffle"), radius = NULL) {
  method <- match.arg(method)
  data <- as.matrix(data)
  if (!is.numeric(data) || ncol(data) != 2 || nrow(data) < 3) {
    stop("data must contain exactly two numeric columns and at least three rows.", call. = FALSE)
  }
  if (any(!is.finite(data))) {
    stop("data must not contain missing or non-finite values.", call. = FALSE)
  }
  if (method == "tts") {
    if (!missing(nperm)) {
      stop("nperm applies only to method = 'shuffle'; TTS uses radius and all allowed shifts.", call. = FALSE)
    }
    if (is.null(radius)) {
      stop("Specify radius for method = 'tts'; the reference method has no default truncation radius.", call. = FALSE)
    }
    if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) ||
        radius < 0 || radius != floor(radius) || 2 * radius >= nrow(data) - 1) {
      stop("radius must be a nonnegative integer with 2 * radius < nrow(data) - 1.", call. = FALSE)
    }
  } else {
    if (!is.null(radius)) stop("radius applies only to method = 'tts'.", call. = FALSE)
    if (!quicknet_is_positive_integer(nperm)) stop("nperm must be a positive integer.", call. = FALSE)
  }

  derivatives <- apply(data, 2, base::diff)
  standard_deviations <- apply(derivatives, 2, stats::sd)
  if (any(!is.finite(standard_deviations)) || any(standard_deviations == 0)) {
    stop("Both columns must have temporal derivatives with nonzero finite standard deviations.", call. = FALSE)
  }
  normalized <- sweep(derivatives, 2, standard_deviations, "/")
  m <- nrow(normalized)
  x <- normalized[, 1L]
  y <- normalized[, 2L]
  coupling <- array(c(x * x, x * y, x * y, y * y), dim = c(m, 2L, 2L))
  result <- list(coupling = coupling, coupling_mean = mean(x * y), p.value = NULL,
                 method = method)

  if (method == "tts") {
    radius <- as.integer(radius)
    indices <- seq.int(radius + 1L, m - radius)
    # Same shift order as the authors' S1 Code: zero, positive, negative.
    shifts <- c(seq.int(0L, radius), -seq_len(radius))
    null <- vapply(shifts, function(shift) mean(x[indices] * y[indices + shift]), numeric(1))
    observed <- null[[1L]]
    extreme <- sum(abs(null) >= abs(observed))
    result$p.value <- min(1, extreme / (radius + 1))
    result$radius <- radius
    result$shifts <- shifts
    result$tts_bound <- extreme / (radius + 1)
    result$minimum_p <- 1 / (radius + 1)
    result$assumption <- paste("Independent first-difference processes under the null;",
      "first differences of the second column are strictly stationary; radius chosen in advance.")
  } else {
    indices <- seq_len(m)
    observed <- result$coupling_mean
    null <- vapply(seq_len(nperm), function(i) {
      dy <- diff(sample(data[, 2L]))
      scale <- stats::sd(dy)
      if (!is.finite(scale) || scale == 0) {
        stop("A shuffled series has zero or non-finite temporal-derivative SD; no p value computed.", call. = FALSE)
      }
      mean(x * dy / scale)
    }, numeric(1))
    extreme <- sum(abs(null) >= abs(observed))
    result$p.value <- (extreme + 1) / (nperm + 1)
    result$nperm <- nperm
    result$minimum_p <- 1 / (nperm + 1)
    result$assumption <- paste("The second column's raw observations are exchangeable under arbitrary",
      "row permutations and independent of the first series under the null.")
  }
  result$test_coupling_mean <- observed
  result$statistic <- abs(observed)
  result$null_coupling <- null
  result$extreme_count <- extreme
  result$derivative_indices <- indices
  result
}


