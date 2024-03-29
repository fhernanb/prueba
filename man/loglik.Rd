% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglik.R
\name{loglik}
\alias{loglik}
\title{Log-likelihood}
\usage{
loglik(x, dist = "dnorm", param = list(mean = 0, sd = 1))
}
\arguments{
\item{x}{numeric vector.}

\item{dist}{density name for the distribution in the form \code{dxxx}. By default it's value is \code{dnorm} but can be used any density.}

\item{param}{list with the parameter values at which can be calculated the log-likelihood.}
}
\description{
This function calculates the log-likelihood at some parameter values given the random sample and distribution.
}
\examples{
# For Poisson distribution
x <- rpois(n=50, lambda=4.5)
loglik(x, dist='dpois', param=list(lambda=4))
loglik(x, dist='dpois', param=list(lamb=4.5))
loglik(x, dist='dpois', param=list(lambda=5))

# For normal distribution
y <- rnorm(n=100, mean=170, sd=10)
loglik(y, dist='dnorm', param=list(mean=170, sd=10))

# Un ejemplo invocando los datos de este paquete
data(condom)
dim(condom)

data(misdatos)
misdatos

}
