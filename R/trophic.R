#' Calculate mean trophic level
#'
#' @description Calulates mean trophic level, by year, site, and transect, for a given location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, and Abundancia. A column for trophic level is optional, though heavily suggested to avoid errors in calculations (this package might not have trophic level information for all species).
#' @param tl An optional dataframe with a column for each species (called GenusSpecies) and a column for the trophic level of each species (called TL).
#' @param location A quoted string that indicates the location.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Sitio, Transecto, GeneroEspecie, and mean trophic level (TL).
#'
#' @export

trophic <- function(data, location){
  library(dplyr)
  library(tidyr)
  library(reshape)

  data <- filter(data, Comunidad == location) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto) %>%
    summarize(mean = mean(NT, na.rm=TRUE))


  return(as.data.frame(data))

}
