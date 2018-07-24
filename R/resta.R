#' resta
#' 
#' Esta funcion sirve para obtener la resta de dos numeros reales.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' resta(1, 1)
#' resta(10, 1)
#' @export
resta <- function(x, y) {
  varianza <- var(c(x, y))
  res <- x + y
  res
}
