#' Calculate density
#'
#' @description Calculates species density (organisms / transect), by year, site, and transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, Abundancia.
#' @param location A quoted string that indicates the location.
#' @param species A quoted string that indicates a species for which density should be calulated.
#'
#'@return D A dataframe with columns for Ano, Zonificacion, Sitio, Transecto, GeneroEspecie, and D (density; org/m2).
#'@author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'
#'@export

density <- function(data, location, species = NULL){
  library(dplyr)  # Load dplyr
  library(tidyr)  # Load tidyr

  # Evaluate if it must calculate density ofr a single species or for all species
  if(is.null(species)){
    D <- data %>%                    #Set D equal to data
      filter(Comunidad == location) %>%      # filter by location
      group_by(Ano,
               Zonificacion,
               Sitio,
               Transecto) %>% #Group by year, zone, transect number and species
      summarize(D = sum(Abundancia)/60)       #Calculate abundance by species by transect
  } else { #Else indicates that a single species has been selected
    D <- data %>%                    #Set D equal to data
      filter(Comunidad == location) %>%      #Filter by location
      filter(GeneroEspecie == species) %>% #Filter by species
      group_by(Ano,
               Zonificacion,
               Sitio,
               Transecto) %>% #Group by year, zone, transect number and species
      summarize(D = sum(Abundancia)/60)
  }

  return(as.data.frame(D))                       #Return D
}
