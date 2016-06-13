#' Calculate biomass
#'
#' @description Calulates biomass for a selected species or the entire dataset, by year and transect, for each location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#' @param species A quoted string that indicates a species for which density should be calulated. Common cases include "Paralabrax clathratus", "Semicossipjus pulcher", or "Anisotremus davidsonii".
#'
#' @return size A dataframe containing year, zone (insido or outside the reserve), transect number, species, and the statistic required.
#'
#' @export

fish_biomass=function(data, site, species=NULL){
  library(dplyr)   #Load dplyr package
  library(tidyr)   #Load tidyr package
  library(reshape) #Load reshape package

  data(abtl)       #Load the database of allometric  cgrowth parameters and trophic level

  data=untable(df=data, n=data$Abundancia) %>% #Untable the data based on Abundance (one line per organism)
    left_join(abtl, by="GeneroEspecie") %>%   #Join data with the database that as a, b and TL values
    mutate(W=a*(SizeClass^b))                 #Create Weight variable

  if(is.null(species)){ #If a single species is not targeted, calculates biomass for all species
    B=data %>% #Set b equals to data
      filter(Site==site) %>%                #Filter by site
      group_by(Year,
               Zone,
               TransectNumber,
               GeneroEspecie) %>%          #Group by Year, Zone, Transect, and GenusSpeices
      summarize(B=sum(W))                    #Create a sum of weight by species
  } else {                                 #If a species is selected
    B=data %>%                             #Set B equals to data
      filter(Site==site) %>%               #Filter by side
      filter(GeneroEspecie==species) %>%   #Filter by species
      group_by(Year,
               Zone,
               TransectNumber,
               GeneroEspecie) %>%         #Group by year, zone, transect number, and species
      summarize(B=sum(W))                   #Create a sum of the weight for selected species
  }

  return(B)                               #Return B

}
