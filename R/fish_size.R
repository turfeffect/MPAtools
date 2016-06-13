#' Calculate a descriptor for fish size
#'
#' @description Calulates mean, median, maximum, or minimum size (cm) for a list of species (or selected species, as indicated by parameter species) by year, and transect, for a speciic location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#' @param species A quoted string that indicates a species for which density should be calulated. Common cases include "Paralabrax clathratus", "Semicossipjus pulcher", or "Anisotremus davidsonii".
#' @param stat A quoted string that indicates which statistic should be calculated. Options include "max", "min", "mean", and "median". User can also use "all" to calculate all of the above.
#'
#' @return size A dataframe containing year, zone (insido or outside the reserve), transect number, species, and the statistic required.
#'
#' @export

fish_size=function(data, site, stat="mean", species=NULL){
  library(dplyr)
  library(tidyr)
  library(reshape)

  data=untable(df=data, n=data$Abundancia)

  if(is.null(species)){
    size=data %>%
      filter(Site==site) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie)
    if (stat=="mean"){
      size=summarize(size,stat=mean(SizeClass))
    } else if (stat=="median"){
      size=summarize(size, stat=median(SizeClass))
    } else if (stat=="max"){
      size=summarize(size, stat=max(SizeClass))
    } else if (stat=="min"){
      size=summarize(size, stat=min(SizeClass))
    } else {
      size=summarize(size,
                     mean=mean(SizeClass),
                     median=median(SizeClass),
                     max=max(SizeClass),
                     min=min(SizeClass))
    }

  } else {
    size=data %>%
      filter(Site==site) %>%
      filter(GeneroEspecie==species) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie)
    if (stat=="mean"){
      size=summarize(size,stat=mean(SizeClass))
    } else if (stat=="median"){
      size=summarize(size, stat=median(SizeClass))
    } else if (stat=="max"){
      size=summarize(size, stat=max(SizeClass))
    } else if (stat=="min"){
      size=summarize(size, stat=min(SizeClass))
    } else {
      size=summarize(size,
                     mean=mean(SizeClass),
                     median=median(SizeClass),
                     max=max(SizeClass),
                     min=min(SizeClass))
    }
  }
}
