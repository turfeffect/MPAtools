#' Calculate biomass
#'
#' @description Calulates biomass for a selected species or the entire dataset, by year, site, and transect, for a given location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie. Columns for a and b parameters are optional, though heavily suggested to avoid errors in calculations (this package might not have all parameters for all species).
#' @param ab An optional dataframe that contains the a and b allometric parameters for each species. Dataframe must have three columns: GeneroEspecie, a, b
#' @param location A quoted string that indicates the location.
#' @param species A quoted string that indicates a species for which density should be calulated.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Sitio, Transecto, GeneroEspecie, and fish biomass (B; in grams).
#'
#' @export

fish_biomass <- function(data, location, species = NULL){
  library(dplyr)   #Load dplyr package
  library(tidyr)   #Load tidyr package

  #If no ab database is passed, checks to see if the data passed has it within the columns or loads default ab database

  data <- data %>%   #Join data with the database that has a and b parameters
    mutate(W = Abundancia*a*(Talla^b))                 #Create Weight variable

  if(is.null(species)){ #If a single species is not targeted, calculates biomass for all species
    B <- data %>% #Set b equals to data
      filter(Comunidad == location) %>%                #Filter by location
      group_by(Ano,
               Zonificacion,
               Sitio,
               Transecto) %>%          #Group by Ano, Zonificacion, Transect, and GenusSpeices
      summarize(B = sum(W, na.rm = T))                    #Create a sum of weight by species
  } else {                                 #If a species is selected
    B <- data %>%                             #Set B equals to data
      filter(Comunidad == location) %>%               #Filter by side
      filter(GeneroEspecie == species) %>%   #Filter by species
      group_by(Ano,
               Zonificacion,
               Sitio,
               Transecto,
               GeneroEspecie) %>%         #Group by year, zone, site, transect number, and species
      summarize(B = sum(W, na.rm = T))                   #Create a sum of the weight for selected species
  }

  return(as.data.frame(B))                               #Return B

}
