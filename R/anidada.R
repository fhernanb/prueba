#' multi
#' 
#' Esta funcion sirve para obtener la multiplicacion de dos numeros reales.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' multi(1, 1)
#' multi(10, 1)
#' @export
multi <- function(x, y) {
  res <- myaux(x, y)
  res
}
#' @export
myaux <- function(x, y) {
  x * y
}

