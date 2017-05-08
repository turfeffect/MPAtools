#' Calculate biomass
#'
#' @description Calulates biomass for a selected species or the entire dataset, by year, site, and transect, for a given location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zona (e.g. "Zona de pesca" or "Zona de no pesca"), Sitio, Transecto, GeneroEspecie. Columns for a and b parameters are optional, though heavily suggested to avoid errors in calculations (this package might not have all parameters for all species).
#' @param a,b An optional dataframe that contains the a and b allometric parameters for each species. Dataframe must have three columns: GeneroEspecie, a, b
#' @param location A quoted string that indicates the location.
#' @param species A quoted string that indicates a species for which density should be calulated.
#'
#' @return size A dataframe with columns for Ano, Zona, Sitio, Transecto, GeneroEspecie, and fish biomass (B; in grams).
#'
#' @export

fish_biomass <- function(data, location, species = NULL){
  library(dplyr)   #Load dplyr package
  library(tidyr)   #Load tidyr package

  #If no ab database is passed, checks to see if the data passed has it within the columns or loads default ab database

  columns <- colnames(data)

  if (!any(columns == "a") & !any(columns == "b")){
    data("abnt")
    data <- left_join(data, abnt, by = "GeneroEspecie")
  }

  data <- data %>%   #Join data with the database that has a and b parameters
    mutate(W = Abundancia*a*(Talla^b)/1000)                 #Create Weight variable

  if(is.null(species)){ #If a single species is not targeted, calculates biomass for all species
    B <- data %>% #Set b equals to data
      filter(Comunidad == location) %>%                #Filter by location
      group_by(Ano,
               Zona,
               Sitio,
               Transecto,
               Temperatura,
               Visibilidad,
               ProfundidadInicial) %>%         #Group by year, zone, site, transect number, and species
      summarize(B = sum(W, na.rm = T)/60) %>%
      ungroup() %>%
      select(Ano, Zona, Sitio, Transecto, Indicador = B, Temperatura, Visibilidad, Profundidad = ProfundidadInicial)

  } else {                                 #If a species is selected
    B <- data %>%                             #Set B equals to data
      filter(Comunidad == location) %>%               #Filter by side
      filter(GeneroEspecie == species) %>%   #Filter by species
      group_by(Ano,
               Zona,
               Sitio,
               Transecto,
               Temperatura,
               Visibilidad,
               ProfundidadInicial) %>%         #Group by year, zone, site, transect number, and species
      summarize(B = sum(W, na.rm = T)/60) %>%
      ungroup() %>%
      select(Ano, Zona, Sitio, Transecto, Indicador = B, Temperatura, Visibilidad, Profundidad = ProfundidadInicial)
  }

  return(as.data.frame(B))                               #Return B

}
