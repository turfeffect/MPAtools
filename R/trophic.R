#' Calculate mean trophic level
#'
#' @description Calulates mean trophic level, by year and transect, for each location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Transecto, GeneroEspecie, and Abundancia.
#' @param tl A dataframe with a column for each species (called GenusSpecies) and a column for the trophic level of each species (called TL).
#' @param location A quoted string that indicates the location.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Transect Number, Species, and mean trophic level.
#'
#' @export

trophic <- function(data, tl = NULL, site){
  library(dplyr)
  library(tidyr)
  library(reshape)

  if(is.null(tl)){ #If no ab database is passed, loads default ab database
    data(abtl)       #Load the database of allometric  cgrowth parameters and trophic level
  }

  data <- untable(df=data, n=data$Abundancia) %>%
    left_join(abtl, by="GeneroEspecie")

  B <- data %>%
    filter(Site == site) %>%
    group_by(Ano, Zonificacion, Transecto) %>%
    summarize(mean = mean(TL, na.rm=TRUE))


  return(B)

}
