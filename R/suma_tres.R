#' Title
#'
#' @param a primer valor.
#' @param b segundo valor.
#' @param c tercer valor.
#'
#' @return un resultado
#' @export
#'
#' @examples
#' suma_tres(a=3, b=5, c=9)
#' multi_tres(a=6, b=2, c=8)
suma_tres <- function(a, b, c) {
  res <- a + b + c
  return(res)
}
#' @rdname suma_tres
#' @export
multi_tres <- function(a, b, c) {
  res <- a * b * c
  return(res)
}