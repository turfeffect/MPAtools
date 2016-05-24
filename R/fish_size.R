#' Calculate a descriptor for fish size
#'
#' @description Calulates central tendency descriptors (mean, mode, median), maximum, or minimum size (cm) for a list of species (or selected species, as indicated by parameter species) by year, and transect, for a speciic location.
#'
#'

fish_size=function(data, site, stat="mean", species=NULL){
  library(dplyr)
  library(tidyr)

  if(is.null(species)){
    size=data %>%
      filter(Site==site) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie)
    if (stat=="mean"){
      size=summarize(size,mean=mean())
    }

  } else {
    size=data %>%
      filter(Site==site) %>%
      group_by(Year, Zone, TransectNumber, GeneroEspecie)
    if (stat=="mean"){
      size=summarize(size,mean=mean())
    }
  }

}
