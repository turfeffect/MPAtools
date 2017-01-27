#' Title
#'
#' @param data
#' @param location
#' @param type
#' @param species
#'
#' @return
#' @export
#'
#'

landings <- function(data, location, type, species = NULL) {
  library(dplyr)
  library(tidyr)

  if (type == "kg") {
    if (is.null(species)) {
      D = data %>%
        filter(Comunidad == location) %>%
        group_by(Ano) %>%
        summarise(Arribos = sum(Arribos))
    }
    else {
      D = data %>%
        filter(Comunidad == location) %>%
        filter(GeneroEspecie == species) %>%
        group_by(Ano, GeneroEspecie) %>%
        summarise(Arribos = sum(Arribos))

    }
  }
  if (type == "price") # Falta corregir por CPI
  {
    if (is.null(species)) {
      D = data %>%
        filter(Comunidad == location) %>%
        group_by(Ano) %>%
        summarise(Precio = sum(Ingresos))
    }
    else {
      D = data %>%
        filter(Comunidad == location) %>%
        filter(GeneroEspecie == species) %>%
        group_by(Ano, GeneroEspecie) %>%
        summarise(Precio = sum(Ingresos))
    }
  }

  return(D)

}
