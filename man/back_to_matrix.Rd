% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/back_to_matrix.R
\name{back_to_matrix}
\alias{back_to_matrix}
\title{Convert a 3-column result of edge difference back to matrix}
\usage{
back_to_matrix(x)
}
\arguments{
\item{x}{a data frame named einv.pals from NCT result object}
}
\value{
a p value matrix with the same dimension with correlation matrix
}
\description{
Convert a 3-column result of edge difference back to matrix
}
\examples{
# uncomment to run
# EBICglassoNet(mtcars) -> net1
# EBICglassoNet(mtcars^3) -> net2
# suppressMessages(NetworkComparisonTest::NCT(net1,net2,it=500, binary.data=FALSE,test.edges=TRUE, edges='all',progressbar = FALSE)) -> NCT
# back_to_matrix(NCT$einv.pvals)

}
