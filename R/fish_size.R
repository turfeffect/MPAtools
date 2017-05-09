#' Calculate a descriptor for fish size
#'
#' @description Calulates mean, median, maximum, or minimum size (cm) for a list of species (or selected species, as indicated by parameter species) by year, site, and transect, for a speciic location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zona (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, Abundancia.
#' @param location A quoted string that indicates the location.
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

  densidad_todos <- density(data, location, species)

  columns <- colnames(data)

  if (!any(columns == "Lm")){
    data("species_bio")
    data <- left_join(data, species_bio, by = "GeneroEspecie")
  }

  data <- data %>%
    filter(GeneroEspecie == species,
           Comunidad == location,
           Talla > Lm) %>%
    group_by(Ano,
             Zona,
             Sitio,
             Transecto) %>%
    summarize(DLT50 = sum(Abundancia, na.rm = T)) %>%
    ungroup() %>%
    right_join(densidad_todos, by = c("Ano", "Zona", "Sitio", "Transecto")) %>%
    mutate(Ni = DLT50/Indicador*100) %>%
    dplyr::select(Ano, Zona, Sitio, Transecto, Indicador = Ni, Temperatura, Visibilidad, Profundidad)

  data$Indicador[is.na(data$Indicador)] <- 0

  return(data)

}
