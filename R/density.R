#' Calculate density
#'
#' @description Calculates species density, by year and transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#' @param species A quoted string that indicates a species for which density should be calulated. Common cases include "Paralabrax clathratus", "Semicossipjus pulcher", or "Anisotremus davidsonii".
#'
#'@return D A dataframe with columns for Year, Zone (inside outside the reserve), Transect Number, Species, and Density (org/m2).
#'@author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'
#'@export

density=function(data, site, species=NULL){
  library(dplyr)  # Load dplyr
  library(tidyr)  # Load tidyr

  # Evaluate if it must calculate density ofr a single species or for all species
  if(is.null(species)){
    D=data %>%                    #Set D equal to data
      filter(Site==site) %>%      # filter by site
      group_by(Year,
               Zone,
               TransectNumber,
               GeneroEspecie) %>% #Group by year, zone, transect number and species
      summarize(D=n()) %>%        #Calculate abundance by species by transect
      mutate(D=D/60)              #Divide by 60 (transects aer 30 m long and 2 m wide).
  } else { #Else indicates that a single species has been selected
    D=data %>%                    #Set D equal to data
      filter(Site==site) %>%      #Filter by site
      filter(GeneroEspecie==species) %>% #Filter by species
      group_by(Year,
               Zone,
               TransectNumber,
               GeneroEspecie) %>% #Group by year, zone, transect number and species
      summarize(D=n()) %>%        #Calculate abundance
      mutate(D=D/60)              #Calculate density
  }

  return(D)                       #Return D
}
