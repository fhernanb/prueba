#' media
#' 
#' Esta funcion sirve para obtener la media de un vector.
#' 
#' @param x A vector.
#' @return The mean of a vector.
#' @examples
#' x <- 1:7
#' media(x)
#' mean(x)
#' @export
#' @useDynLib prueba
#' @importFrom Rcpp sourceCpp
media <- function(x) {
  meanC(x)
}
