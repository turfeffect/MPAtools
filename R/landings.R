#' Title
#'
#' @param data A data.frame with landings and income data
#' @param location The community of interest
#' @param type Specify if the function must calculate "price" for income or "kg" for landings
#' @param species An OPTIONAL species of interest
#'
#' @return A data.frame with the Landings or Income per year
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
