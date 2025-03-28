% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{h_get_vars}
\alias{h_get_vars}
\title{Extract Variable Names}
\usage{
h_get_vars(treatment)
}
\arguments{
\item{treatment}{(\code{string} or \code{formula}) string name of the treatment, or a formula.}
}
\value{
A list of three elements, \code{treatment}, \code{schema} and \code{strata}.
}
\description{
Extract Variable Names
}
\details{
Extract the formula elements, including \code{treatment}, \code{schema} and \code{strata}.
}
\keyword{internal}
