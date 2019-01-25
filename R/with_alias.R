#' add
#' 
#' Esta funcion sirve para obtener la suma de dos numeros reales.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
add <- function(x, y) {
  res <- x + y
  res
}
#'
#' hola1
#' 
#' Esta funcion saluda en forma 1
#' 
#' @param nombre el nombre del usuario
#' @examples
#' hola1('olga')
#' @export
hola1 <- function(nombre) {
  paste('hola ', nombre, ' buen dia')
}
#' @rdname hola1
#' @export
hola2 <- function(nombre) {
  paste('hello ', nombre, ' good day')
}
