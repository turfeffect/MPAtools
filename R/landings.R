#################################################
# Landings function
# Caio Faro
# 10/06
################################################


landings <- function(data, site, type, species = NULL)
{
  library(dplyr)
  library(tidyr)

  if (type == "kg") {
    if (is.null(species)) {
    D = data %>%
    filter(UnidadEconomica = site) %>%
    group_by(Ano, NombreCientifico) %>%
    summarise(Peso = sum(PesoVivo))
    }
    else {
      D = data %>%
      filter(UnidadEconomica == site) %>%
      filter(NombreCientifico == species) %>%
        group_by(Ano, NombreCientifico) %>%
        summarise(Peso = sum(PesoVivo))

    }
  }
  if (type == "price")
  {
    if (is.null(species)) {
      D = data %>%
        filter(UnidadEconomica == site) %>%
        group_by(Ano, NombreCientifico) %>%
        summarise(Precio = sum(Valor))
    }
    else {
      D = data %>%
        filter(UnidadEconomica = site) %>%
        filter(NombreCientifico = species) %>%
        group_by(Ano, NombreCientifico) %>%
        summarise(Precio = sum(Valor))
    }
  }

  return(D)

}

