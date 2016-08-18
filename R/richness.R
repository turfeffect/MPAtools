#' Calculate species richness
#'
#' @description Calculates species richness, by year, site, and transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie.
#' @param location A quoted string that indicates the location.
#'
#'@return S A dataframe with columns for Ano, Zonificacion, Sitio, Transecto, and Species richness (S; number of species).
#'
#'@export

richness <- function(data, location){
  library(dplyr) #Load dplyr
  library(tidyr) #Load tidyr

  S <- data %>% #Set S equal to data
    filter(Comunidad == location) %>% #Filter by location
    filter(Abundancia > 0) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto,
             GeneroEspecie) %>%
    summarize(N = n()) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto) %>%
    summarize(S = n())

  return(S)
}
