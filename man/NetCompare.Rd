% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NetCompare.R
\name{NetCompare}
\alias{NetCompare}
\title{Statistical Comparison of Two Networks Based on Three Invariance Measures}
\usage{
NetCompare(
  data1,
  data2,
  it = 5000,
  binary.data = FALSE,
  paired = FALSE,
  weighted = TRUE,
  AND = TRUE,
  abs_edge = TRUE,
  test.edges = TRUE,
  edges = "all",
  progressbar = TRUE,
  make.positive.definite = TRUE,
  p.adjust.methods = "none",
  test.centrality = TRUE,
  centrality = "all",
  nodes = "all",
  add.bridge = FALSE,
  communities = NULL,
  useCommunities = "all",
  sig.level = 0.05,
  ...
)
}
\arguments{
\item{data1}{One of two datasets. The dimension of the matrix is nobs x nvars; each row is a vector of observations of the variables. Must be cross-sectional data. Can also be the result of estimateNetwork from the bootnet package.}

\item{data2}{The other of two datasets. The dimension of the matrix is nobs x nvars; each row is a vector of observations of the variables. Must be cross-sectional data. Can also be the result of estimateNetwork from the bootnet package.}

\item{it}{The number of iterations (permutations).}

\item{binary.data}{Logical. Can be TRUE or FALSE to indicate whether the data is binary or not. If binary.data is FALSE, the data is regarded gaussian.}

\item{paired}{Logical. Can be TRUE of FALSE to indicate whether the samples are dependent or not. If paired is TRUE, relabeling is performed within each pair of observations. If paired is FALSE, relabeling is not restricted to pairs of observations. Note that, currently, dependent data is assumed to entail one group measured twice.}

\item{weighted}{Logical. Can be TRUE of FALSE to indicate whether the networks to be compared should be weighted of not. If not, the estimated networks are dichotomized. Defaults to TRUE.}

\item{AND}{Logical. Can be TRUE of FALSE to indicate whether the AND-rule or the OR-rule should be used to define the edges in the network. Defaults to TRUE. Only necessary for binary data.}

\item{test.edges}{Logical. Can be TRUE of FALSE to indicate whether or not differences in individual edges should be tested.}

\item{edges}{Character or list. When 'all', differences between all individual edges are tested. When provided a list with one or more pairs of indices referring to variables, the provided edges are tested.}

\item{progressbar}{Logical. Should the pbar be plotted in order to see the progress of the estimation procedure? Defaults to TRUE.}

\item{make.positive.definite}{If \code{make.positive.definite = TRUE}, the covariance matrices used for the glasso are projected to the nearest positive definite matrices, if they are not yet positive definite. This is useful for small n, for which it is very likely that at least one of the bootstrap comparisons involves a covariance matrix that is not positive definite.}

\item{p.adjust.methods}{Character. Can be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none". To control (or not) for testing of multiple edges. Defaults to "none".}

\item{add.bridge}{a logical value to calculate the difference of  bridge coefficients or not. If the value is TRUE, "bridgeStrength", "bridgeCloseness", "bridgeBetweenness", "bridgeExpectedInfluence" will be added to the results.}

\item{communities}{used for bridge centrality measures. If add.bridge is set TRUE, this should be provided. Note: should only be a numeric vector with the same length of nodes, the number indicates the community that each community belongs to.}

\item{useCommunities}{character vector specifying which communities should be included. Default set to "all".}

\item{sig.level}{significance level of the test, this only affect the output of diff_sig_nw1>nw2 and diff_sig_nw1<nw2.}

\item{...}{other paramaters from \code{NetworkComparisonTest::NCT}}

\item{abs}{Logical. Should global strength consider the absolute value of edge weights, or the raw value (i.e., global expected influence)?}
}
\value{
returns a 'NCT' object that contains the following items:\itemize{
\item\code{glstrinv.real:} The difference in global strength between the networks of the observed data sets.
\item\code{glstrinv.perm:} The difference in global strength between the networks of the permutated data sets.
\item\code{glstrinv.sep:} The global strength values of the individual networks.
\item\code{glstrinv.pval:} The p value resulting from the permutation test concerning difference in global strength.
\item\code{nwinv.real:} The value of the maximum difference in edge weights of the observed networks.
\item\code{nwinv.perm:} The values of the maximum difference in edge weights of the permuted networks.
\item\code{nwinv.pval:} The p value resulting from the permutation test concerning the maximum difference in edge weights.
\item\code{einv.pvals:} p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
\item\code{edges.tested:} The pairs of variables between which the edges are called to be tested. Only if test.edges = TRUE.
\item\code{einv.real:} The value of the difference in edge weight of the observed networks (multiple values if more edges are called to test). Only if test.edges = TRUE.
\item\code{einv.perm:} The values of the difference in edge weight of the permuted networks. Only if test.edges = TRUE.
\item\code{diffcen.real:} The values of the difference in centralities of the observed networks. Only if test.centrality = TRUE.
\item\code{diffcen.perm:} The values of the difference in centralities of the permuted networks. Only if test.centrality = TRUE.
\item\code{diffcen.pval:} p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
\item\code{edge_weight_p} the wide format of einv.pvals, p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
\item\code{diff_sig_nw1>nw2:} the value of significant edge weight differences by nw1-nw2, unsignificant edge weights are set as 0.
\item\code{diff_sig_nw1<nw2:} the value of significant edge weight differences by nw2-nw1, unsignificant edge weights are set as 0.
\item\code{net1_mask:} a binary matrix that indicates non-zero edges in the EBICglasso network constructed from data1.
\item\code{net2_mask:} a binary matrix that indicates non-zero edges in the EBICglasso network constructed from data2.
}
}
\description{
a wrapper of \code{NetworkComparisonTest::NCT}
}
\examples{
data('mtcars')
NetCompare(mtcars, mtcars^3, it = 100)

NetCompare(mtcars, mtcars^3, it = 100, add.bridge = TRUE, communities = c(rep(1,4),rep(2,4),rep(3,3)), useCommunities = c(1,2))

}
