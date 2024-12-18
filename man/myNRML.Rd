% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myNRML.R
\name{myNRML}
\alias{myNRML}
\title{Newton Raphson Method}
\usage{
myNRML(x0, delta = 0.001, llik, xrange, parameter = "param")
}
\arguments{
\item{x0}{Initial estimate.}

\item{delta}{Timestep for the derivative.}

\item{llik}{Log-likelihood function.}

\item{xrange}{X range of the graph produced by this function.}

\item{parameter}{Main title of the graph.}
}
\value{
A list containing the estimated parameters and their derivatives.
}
\description{
Estimates the likelihood of a parameter.
}
\examples{
llik_example <- function(x) {
  log(dpois(4, x) * dpois(6, x) * dpois(7, x) * dpois(6, x) * dpois(5, x))
}
myNRML(x0 = 2, delta = 0.000001, llik = llik_example, xrange = c(0, 20), parameter = "lambda")
}
