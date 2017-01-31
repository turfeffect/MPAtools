#' Calculate a descriptor for fish size
#'
#' @description Calulates mean, median, maximum, or minimum size (cm) for a list of species (or selected species, as indicated by parameter species) by year, site, and transect, for a speciic location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zona (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, Abundancia.
#' @param location A quoted string that indicates the location.
#' @param stat A quoted string that indicates which statistic should be calculated. Options include "max", "min", "mean", and "median". User can also use "all" to calculate all of the above.
#' @param species A quoted string that indicates a species for which the size statistic should be calulated.
#'
#' @return size A dataframe with columns for Ano, Zona, Sitio, Transecto, GeneroEspecie, and the statistic required.
#'
#' @export

fish_size <- function(data, location, species = NULL){
  library(dplyr)
  library(tidyr)

  if (is.null(species)){
    stop("Pleae specify an objective species")
  }

  densidad_todos <- MPAtools::density(data, location, species)

  if (!any(columns == "LT50")){
    data("abnt")
    abnt$LT50 <- 45
    data <- left_join(data, abnt, by = "GeneroEspecie")
  }

  data <- filter(data, GeneroEspecie == species,
                 Comunidad == location,
                 Talla > LT50) %>%
  group_by(Ano,
           Zona,
           Sitio,
           Transecto) %>%
    summarize(DLT50 = sum(Abundancia, na.rm = T)) %>%
    right_join(densidad_todos, by = c("Ano", "Zona", "Sitio", "Transecto")) %>%
    mutate(Ni = DLT50/D*100) %>%
    select(Ano, Zona, Sitio, Transecto, Ni, Temperatura, Visibilidad, Profundidad)

  data$Ni[is.na(data$Ni)] <- 0

  return(data)

}
