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
#' asbio::shade.norm(x=1.2,sigma=1,mu=0,tail="lower")
#' @export
multi <- function(x, y) {
  res <- myaux(x, y)
  res
}
#' @rdname multi
#' @export
myaux <- function(x, y) {
  x * y
}

