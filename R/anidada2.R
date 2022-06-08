#' multi2
#' 
#' Esta funcion sirve para obtener la multiplicacion de dos numeros reales.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' multi2(1, 1)
#' multi2(10, 1)
#' @export
multi2 <- function(x, y) {
  res <- myaux2(x, y)
  res
}
#' @importFrom stats rnorm
myaux2 <- function(x, y) {
  #muestra <- rnorm(n=10)
  x * y
}

