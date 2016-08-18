#' Calculate biomass
#'
#' @description Calulates biomass for a selected species or the entire dataset, by year and transect, for each location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Transecto, GeneroEspecie.
#' @param ab A dataframe that contains the a and b allometric parameters for each species. Dataframe must have three columns: GeneroEspecie, a, b
#' @param location A quoted string that indicates the location.
#' @param species A quoted string that indicates a species for which density should be calulated.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Transect Number, Species, and fish biomass.
#'
#' @export

fish_biomass <- function(data, ab = NULL, location, species = NULL){
  library(dplyr)   #Load dplyr package
  library(tidyr)   #Load tidyr package
  library(reshape) #Load reshape package

  if(is.null(ab)){ #If no ab database is passed, loads default ab database
    data(abtl)       #Load the database of allometric  cgrowth parameters and trophic level
  }

  data <- data %>% #Untable the data based on Abundance (one line per organism)
    left_join(ab, by = "GeneroEspecie") %>%   #Join data with the database that as a, b and TL values
    mutate(W = Abundancia*a*(SizeClass^b))                 #Create Weight variable

  if(is.null(species)){ #If a single species is not targeted, calculates biomass for all species
    B <- data %>% #Set b equals to data
      filter(Comunidad == location) %>%                #Filter by location
      group_by(Ano,
               Zonificacion,
               Transecto,
               GeneroEspecie) %>%          #Group by Ano, Zonificacion, Transect, and GenusSpeices
      summarize(B = sum(W))                    #Create a sum of weight by species
  } else {                                 #If a species is selected
    B <- data %>%                             #Set B equals to data
      filter(Comunidad == location) %>%               #Filter by side
      filter(GeneroEspecie == species) %>%   #Filter by species
      group_by(Ano,
               Zonificacion,
               Transecto,
               GeneroEspecie) %>%         #Group by year, zone, transect number, and species
      summarize(B = sum(W))                   #Create a sum of the weight for selected species
  }

  return(B)                               #Return B

}
