devtools::load_all()
devtools::document()

devtools::install()

library(prueba)

# Para hacer un chequeo
devtools::check(remote=TRUE)

# Para crear la pagina web
pkgdown::build_site()