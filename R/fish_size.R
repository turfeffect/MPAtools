#' Calculate a descriptor for fish size
#'
#' @description Calulates mean, median, maximum, or minimum size (cm) for a list of species (or selected species, as indicated by parameter species) by year, and transect, for a speciic location.
#'
#' @param data A dataframe that contains at least columns of Ano, Zonificacion (e.g. "Zona de pesca" or "Zona de no pesca"), Transecto, GeneroEspecie, Abundancia.
#' @param location A quoted string that indicates the location.
#' @param stat A quoted string that indicates which statistic should be calculated. Options include "max", "min", "mean", and "median". User can also use "all" to calculate all of the above.
#' @param species A quoted string that indicates a species for which the size statistic should be calulated.
#'
#' @return size A dataframe with columns for Ano, Zonificacion, Transect Number, Species, and the statistic required.
#'
#' @export

fish_size <- function(data, location, stat="mean", species=""){
  library(dplyr)
  library(tidyr)
  library(reshape)

  if (species == ""){
    stop("Pleae specify a species")
  }

  data <- untable(df = data, num = data$Abundancia)

    size <- data %>%
      filter(Comunidad == location) %>%
      filter(GeneroEspecie == species) %>%
      group_by(Ano,
               Zonificacion,
               Transecto,
               GeneroEspecie)

    if (stat == "mean"){
      size <- summarize(size, stat = mean(Talla))
    } else if (stat == "median"){
      size <- summarize(size, stat = median(Talla))
    } else if (stat == "max"){
      size <- summarize(size, stat = max(Talla))
    } else if (stat == "min"){
      size <- summarize(size, stat = min(Talla))
    } else {
      size <- summarize(size,
                     mean = mean(Talla),
                     median = median(Talla),
                     max = max(Talla),
                     min = min(Talla))
    }
}
