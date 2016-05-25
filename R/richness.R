#' Calculate species richness
#'
#' @description Calculates species density, by transect, for a specific location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#'
#'@return S A dataframe with columns for Year, Zone (inside outside the reserve), Transect Number, and Species richness (S).
#'
#'@export

richness=function(data,site){
  library(dplyr) #Load dplyr
  library(tidyr) #Load tidyr

  S=data %>% #Set S equal to data
    filter(Site==site) %>% #Filter by site
    group_by(Year,
             Zone,
             TransectNumber,
             GeneroEspecie) %>%
    summarize(N=n()) %>%
    group_by(Year,
             Zone,
             TransectNumber) %>%
    summarize(S=n())

  return(S)
}
