% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_edges.R
\name{get_edges}
\alias{get_edges}
\title{Get edge index of one or two networks}
\usage{
get_edges(net1, net2 = NULL, method = "union")
}
\arguments{
\item{net1}{a network. Should be either the product of \code{qgraph} or \code{quickNet}.}

\item{net2}{alternative. a network. Should be either the product of \code{qgraph} or \code{quickNet}.}

\item{method}{only works when net2 is provided. \itemize{
\item{"union": return the edges that exist in net1 or net2, default.}
\item{"intersect": return the edges that both exist in net1 and net2.}
}}
}
\value{
a list with n vectors of length 2. The vectors represent the node index. n is the number of edges. The result can be provided to the \code{edges} parameter of \code{NetCompare} or \code{NCT} to specify the edges that you want to test.
}
\description{
Get edge index of one or two networks
}
\examples{

data("mtcars")
net1 <- quickNet(mtcars)
net2 <- quickNet(mtcars^3)
edges1 <- get_edges(net1)
edges2 <- get_edges(net1, net2, method = 'intersect')

}
