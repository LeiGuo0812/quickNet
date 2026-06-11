\name{model_registry}
\alias{model_registry}
\title{Show the quickNet model registry}
\usage{
model_registry(model = NULL)
}
\arguments{
\item{model}{Optional model or function name. If \code{NULL}, all registered
models and analysis modules are returned.}
}
\value{
A data frame with model family, backend, data type, network layers,
reportable quantities, key references, and known limitations.
}
\description{
Show the quickNet model registry.
}
