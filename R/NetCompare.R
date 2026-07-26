#' @title Statistical Comparison of Two Networks Based on Three Invariance Measures
#' @description a wrapper of \code{NetworkComparisonTest::NCT}
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @param data1 One of two datasets. The dimension of the matrix is nobs x nvars; each row is a vector of observations of the variables. Must be cross-sectional data. Can also be the result of estimateNetwork from the bootnet package.
#' @param data2 The other of two datasets. The dimension of the matrix is nobs x nvars; each row is a vector of observations of the variables. Must be cross-sectional data. Can also be the result of estimateNetwork from the bootnet package.
#' @param it The number of iterations (permutations).
#' @param binary.data Logical. Can be TRUE or FALSE to indicate whether the data is binary or not. If binary.data is FALSE, the data is regarded gaussian.
#' @param paired Logical. Can be TRUE of FALSE to indicate whether the samples are dependent or not. If paired is TRUE, relabeling is performed within each pair of observations. If paired is FALSE, relabeling is not restricted to pairs of observations. Note that, currently, dependent data is assumed to entail one group measured twice.
#' @param weighted Logical. Can be TRUE of FALSE to indicate whether the networks to be compared should be weighted of not. If not, the estimated networks are dichotomized. Defaults to TRUE.
#' @param AND Logical. Can be TRUE of FALSE to indicate whether the AND-rule or the OR-rule should be used to define the edges in the network. Defaults to TRUE. Only necessary for binary data.
#' @param abs_edge Logical. Should global strength consider the absolute value of edge weights, or the raw value (i.e., global expected influence)?
#' @param test.edges Logical. Can be TRUE of FALSE to indicate whether or not differences in individual edges should be tested.
#' @param edges Character or list. When 'all', differences between all individual edges are tested. When provided a list with one or more pairs of indices referring to variables, the provided edges are tested.
#' @param progressbar Logical. Should the pbar be plotted in order to see the progress of the estimation procedure? Defaults to TRUE.
#' @param make.positive.definite If \code{make.positive.definite = TRUE}, the covariance matrices used for the glasso are projected to the nearest positive definite matrices, if they are not yet positive definite. This is useful for small n, for which it is very likely that at least one of the bootstrap comparisons involves a covariance matrix that is not positive definite.
#' @param p.adjust.methods Character. Can be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none". To control (or not) for testing of multiple edges. Defaults to "none".
#' @param test.centrality Logical. Should centrality differences be tested?
#' @param centrality Character vector specifying centrality statistics to test.
#' @param nodes Character vector specifying nodes to test.
#' @param add.bridge a logical value to calculate the difference of  bridge coefficients or not. If the value is TRUE, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness", "bridgeExpectedInfluence" will be added to the results.
#' @param communities used for bridge centrality measures. If add.bridge is set TRUE, this should be provided. Note: should only be a numeric vector with the same length of nodes, the number indicates the community that each community belongs to.
#' @param useCommunities character vector specifying which communities should be included. Default set to "all".
#' @param sig.level significance level of the test, this only affect the output of diff_sig_nw1>nw2 and diff_sig_nw1<nw2.
#' @param ... other parameters from \code{NetworkComparisonTest::NCT}
#'
#' @return returns a 'NCT' object that contains the following items:\itemize{
#' \item\code{glstrinv.real:} The difference in global strength between the networks of the observed data sets.
#' \item\code{glstrinv.perm:} The difference in global strength between the networks of the permutated data sets.
#' \item\code{glstrinv.sep:} The global strength values of the individual networks.
#' \item\code{glstrinv.pval:} The p value resulting from the permutation test concerning difference in global strength.
#' \item\code{nwinv.real:} The value of the maximum difference in edge weights of the observed networks.
#' \item\code{nwinv.perm:} The values of the maximum difference in edge weights of the permuted networks.
#' \item\code{nwinv.pval:} The p value resulting from the permutation test concerning the maximum difference in edge weights.
#' \item\code{einv.pvals:} p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
#' \item\code{edges.tested:} The pairs of variables between which the edges are called to be tested. Only if test.edges = TRUE.
#' \item\code{einv.real:} The value of the difference in edge weight of the observed networks (multiple values if more edges are called to test). Only if test.edges = TRUE.
#' \item\code{einv.perm:} The values of the difference in edge weight of the permuted networks. Only if test.edges = TRUE.
#' \item\code{diffcen.real:} The values of the difference in centralities of the observed networks. Only if test.centrality = TRUE.
#' \item\code{diffcen.perm:} The values of the difference in centralities of the permuted networks. Only if test.centrality = TRUE.
#' \item\code{diffcen.pval:} p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
#' \item\code{edge_weight_p} the wide format of einv.pvals, p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
#' \item\code{diff_sig_nw1>nw2:} the value of significant edge weight differences by nw1-nw2, unsignificant edge weights are set as 0.
#' \item\code{diff_sig_nw1<nw2:} the value of significant edge weight differences by nw2-nw1, unsignificant edge weights are set as 0.
#' \item\code{net1_mask:} a binary matrix that indicates non-zero edges in the
#' network estimated from data1.
#' \item\code{net2_mask:} a binary matrix that indicates non-zero edges in the
#' network estimated from data2.
#' }
#' @export
#'
#' @examples
#' data('mtcars')
#' NetCompare(mtcars, mtcars^3, it = 100)
#'
#' NetCompare(
#'   mtcars, mtcars^3, it = 100,
#'   add.bridge = TRUE,
#'   communities = c(rep(1, 4), rep(2, 4), rep(3, 3)),
#'   useCommunities = c(1, 2)
#' )
#'
NetCompare <- function(data1, data2, it = 5000, binary.data=FALSE, paired = FALSE, weighted = TRUE, AND = TRUE, abs_edge = TRUE, test.edges=TRUE, edges='all', progressbar=TRUE, make.positive.definite = TRUE, p.adjust.methods = 'none', test.centrality = TRUE, centrality = 'all', nodes = 'all', add.bridge = FALSE, communities = NULL, useCommunities = 'all',sig.level = 0.05, ...){
  if (!is.logical(add.bridge) || length(add.bridge) != 1 || is.na(add.bridge)) {
    stop("Error: add.bridge should be logical.")
  }
  if (!is.logical(test.edges) || length(test.edges) != 1 || is.na(test.edges)) {
    stop("test.edges should be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(sig.level) || length(sig.level) != 1 || !is.finite(sig.level) ||
      sig.level <= 0 || sig.level >= 1) {
    stop("sig.level must be a finite number in (0, 1).", call. = FALSE)
  }

  centrality_to_test <- if (isTRUE(add.bridge)) {
    centrality
  } else if (length(centrality) == 1 && identical(tolower(centrality), "all")) {
    c("closeness", "betweenness", "strength", "expectedInfluence")
  } else {
    centrality
  }

  results <- NCT_gl(
    data1,
    data2,
    it = it,
    binary.data = binary.data,
    paired = paired,
    weighted = weighted,
    AND = AND,
    abs = abs_edge,
    test.edges = test.edges,
    edges = edges,
    progressbar = progressbar,
    make.positive.definite = make.positive.definite,
    p.adjust.methods = p.adjust.methods,
    test.centrality = test.centrality,
    centrality = centrality_to_test,
    nodes = nodes,
    communities = communities,
    useCommunities = useCommunities,
    ...
  )

  if (is.null(results$info)) results$info <- list()
  if (is.null(results$info$call)) results$info$call <- list()
  results$info$call$abs <- abs_edge
  results$net1_mask <- (results$nw1 != 0) * 1
  results$net2_mask <- (results$nw2 != 0) * 1

  if (!isTRUE(test.edges)) {
    results$edge_weight_p <- NULL
    results$diff_sig <- NULL
    results$`diff_sig_nw1>nw2` <- NULL
    results$`diff_sig_nw1<nw2` <- NULL
    return(results)
  }

  node_names <- colnames(results$nw1)
  p <- matrix(NA_real_, length(node_names), length(node_names), dimnames = list(node_names, node_names))
  diag(p) <- 1
  if (!is.null(results$einv.pvals) && nrow(results$einv.pvals) > 0) {
    for (i in seq_len(nrow(results$einv.pvals))) {
      node_i <- as.character(results$einv.pvals$Var1[[i]])
      node_j <- as.character(results$einv.pvals$Var2[[i]])
      if (node_i %in% node_names && node_j %in% node_names) {
        value <- suppressWarnings(as.numeric(as.character(results$einv.pvals$`p-value`[[i]])))
        p[node_i, node_j] <- p[node_j, node_i] <- value
      }
    }
  }
  results$edge_weight_p <- p

  p_for_mask <- p
  p_for_mask[is.na(p_for_mask)] <- 1
  p_mask <- (p_for_mask < sig.level) * 1
  diff <- results$nw1 - results$nw2
  diff_masked <- diff * p_mask
  results$diff_sig <- diff_masked
  results$`diff_sig_nw1>nw2` <- diff_masked * (diff_masked > 0)
  results$`diff_sig_nw1<nw2` <- diff_masked * (diff_masked < 0)
  results
}
