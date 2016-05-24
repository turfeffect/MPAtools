#' Calculate density
#'
#' @description Calculates species density, by year and transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#' @param species A quoted string that indicates a species for which density should be calulated. Common cases include "Paralabrax clathratus", "Semicossipjus pulcher", or "Anisotremus davidsonii".
#'
#'@return D A dataframe with columns for Year, Zone (inside outside the reserve), Transect Number, Species, and Density (org/m2).

density=function(data, site, species=NULL){
  library(dplyr)
  library(tidyr)

  if(is.null(species)){
    D=data %>%
      filter(Site==site) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie) %>%
      summarize(D=n()) %>%
      mutate(D=D/60)
  } else {
    D=data %>%
      filter(Site==site) %>%
      filter(GeneroEspecie==species) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie) %>%
      summarize(D=n()) %>%
      mutate(D=D/60)
  }

  return(D)
}
