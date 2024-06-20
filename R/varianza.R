#' varianza
#' 
#' Esta funcion sirve para obtener la varianza de un vector.
#' 
#' @param x A vector.
#' @return The variance of a vector.
#' @examples
#' x <- 1:7
#' varianza(x)
#' var(x)
#' @export
#' @useDynLib prueba
#' @importFrom Rcpp sourceCpp
varianza <- function(x) {
  varC(x)
}