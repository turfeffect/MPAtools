#' Calculate species richness
#'
#' @description Calculates species richness, by transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Transecto, GeneroEspecie.
#' @param location A quoted string that indicates the location.
#'
#'@return S A dataframe with columns for Ano, Zonificacion, Transecto, and Species richness (S).
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
             Transecto,
             GeneroEspecie) %>%
    summarize(N = n()) %>%
    group_by(Ano,
             Zonificacion,
             Transecto) %>%
    summarize(S = n())

  return(S)
}
