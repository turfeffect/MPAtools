#' Calculate mean trophic level
#'
#' @description Calulates mean trophic level, by year, site, and transect, for a given location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie, and Abundancia. A column for trophic level is optional, though heavily suggested to avoid errors in calculations (this package might not have trophic level information for all species).
#' @param tl An optional dataframe with a column for each species (called GenusSpecies) and a column for the trophic level of each species (called TL).
#' @param location A quoted string that indicates the location.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Sitio, Transecto, GeneroEspecie, and mean trophic level (TL).
#'
#' @export

trophic <- function(data, tl = NULL, location){
  library(dplyr)
  library(tidyr)
  library(reshape)

  #If no tl dattlase is passed, checks to see if the data passed has it within the columns or loads default tl dattlase
  if(is.null(tl) & any(colnames(data) == "TL")){
    tl <- tl
  } else {
    tl <- data(tltl)       #Load the dattlase of allometric  growth parameters and trophic level
    data <- untable(df = data, num = data$Abundancia) %>% #Untable the data based on Abundance (one line per organism)
      left_join(tl, by = "GeneroEspecie")  #Add TL values for each species
  }

  if(!is.null(tl)){
    data <- untable(df = data, num = data&Abundancia) %>% #Untable the data based on Abundance (one line per organism)
      left_join(tl, by = "GeneroEspecie")  #Add TL values for each species
  }

  B <- data %>%
    filter(Comunidad == location) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto) %>%
    summarize(mean = mean(TL, na.rm=TRUE))


  return(B)

}
