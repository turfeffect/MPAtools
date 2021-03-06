#' Calculate mean trophic level
#'
#' @description Calulates mean trophic level, by year, site, and transect, for a given location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zona (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, and Abundancia. A column for trophic level is optional, though heavily suggested to avoid errors in calculations (this package might not have trophic level information for all species).
#' @param tl An optional dataframe with a column for each species (called GenusSpecies) and a column for the trophic level of each species (called TL).
#' @param location A quoted string that indicates the location.
#'
#' @return size A dataframe with columns for Ano, Zona, Sitio, Transecto, GeneroEspecie, and mean trophic level (TL).
#'
#' @export

trophic <- function(data, location){
  library(dplyr)
  library(tidyr)

  columns <- colnames(data)

  if (!any(columns == "NT")){
    data("species_bio")
    data <- left_join(data, species_bio, by = "GeneroEspecie")
  }

  data <- filter(data, Comunidad == location) %>%
    group_by(Ano,
             Zona,
             Sitio,
             Transecto,
             Temperatura,
             Visibilidad,
             ProfundidadInicial) %>%
    summarize(NT = mean(NT, na.rm=TRUE)) %>%
    select(Ano, Zona, Sitio, Transecto, Indicador = NT, Temperatura, Visibilidad, Profundidad = ProfundidadInicial)


  return(as.data.frame(data))

}
