# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Function to obtain F11 with C++.
#' @param gamma numeric value for gamma.
#' @param lambda numeric value for lambda.
#' @param maxiter_series numeric value.
#' @param tol numeric value.
#' @export
#' @return returns the F11 value.
f11_cpp <- function(gamma, lambda, maxiter_series = 10000L, tol = 1.0e-10) {
    .Call('_prueba_f11_cpp', PACKAGE = 'prueba', gamma, lambda, maxiter_series, tol)
}

#' Function to obtain the dHYPERPO for a single value x
#' @param x numeric value for x.
#' @param mu numeric value for nu.
#' @param sigma numeric value for sigma.
#' @param log logical value for log.
#' @export
#' @return returns the pmf for a single value x.
dHYPERPO_single <- function(x, mu = 1, sigma = 1, log = FALSE) {
    .Call('_prueba_dHYPERPO_single', PACKAGE = 'prueba', x, mu, sigma, log)
}

#' Function to obtain the dHYPERPO for a vector x
#' @param x numeric value for x.
#' @param mu numeric value for nu.
#' @param sigma numeric value for sigma.
#' @param log logical value for log.
#' @export
#' @return returns the pmf for a vector.
dHYPERPO_vec <- function(x, mu, sigma, log) {
    .Call('_prueba_dHYPERPO_vec', PACKAGE = 'prueba', x, mu, sigma, log)
}

#' Media de un vector usando C++.
#' @param x numeric vector
#' @export
#' @return la media.
meanC <- function(x) {
    .Call('_prueba_meanC', PACKAGE = 'prueba', x)
}

#' Varianza de un vector usando C++.
#' @param x numeric vector
#' @export
#' @return la varianza.
varC <- function(x) {
    .Call('_prueba_varC', PACKAGE = 'prueba', x)
}

