#' Calculate mean trophic level
#'
#' @description Calulates mean trophic level, by year and transect, for each location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#'
#' @return size A dataframe containing year, zone (insido or outside the reserve), transect number, species, and the statistic required.
#'
#' @export

trophic=function(data, site){
  library(dplyr)
  library(tidyr)
  library(reshape)

  data(abtl)

  data=untable(df=data, n=data$Abundancia) %>%
    left_join(abtl, by="GeneroEspecie")
  B=data %>%
    filter(Site==site) %>%
    group_by(Year, Zone, TransectNumber) %>%
    summarize(mean=mean(TL, na.rm=TRUE))


  return(B)

}
