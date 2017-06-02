#' Title
#'
#' @param data A data.frame with landings and income data
#' @param location The community of interest
#' @param type Specify if the function must calculate "price" for income or "kg" for landings
#' @param species An OPTIONAL species of interest
#'
#' @return A data.frame with the Landings or Income per year
#' @export
#'
#'

landings <- function(data, location, type, species = NULL) {
  library(dplyr)
  library(tidyr)

  if (type == "kg") {
    if (is.null(species)) {
      D = data %>%
        filter(Comunidad == location) %>%
        group_by(Ano) %>%
        summarise(Indicador = sum(Arribos)) %>% 
        ungroup()
    } else {
      D = data %>%
        filter(Comunidad == location) %>%
        filter(GeneroEspecie == species) %>%
        group_by(Ano, GeneroEspecie) %>%
        summarise(Indicador = sum(Arribos)) %>% 
        ungroup()
    }
  }

  if (type == "price") {

    latest_year <- max(data$Ano)
    latest_cpi <- data$CPI[data$Ano == latest_year]

    data <- mutate(data,
                    CPI_Adj = CPI/latest_cpi,
                    Ingresos = Ingresos * CPI_Adj)

    if (is.null(species)) {
      D = data %>%
        filter(Comunidad == location) %>%
        group_by(Ano) %>%
        summarise(Indicador = sum(Ingresos)) %>% 
        ungroup()
    } else {
      D = data %>%
        filter(Comunidad == location) %>%
        filter(GeneroEspecie == species) %>%
        group_by(Ano, GeneroEspecie) %>%
        summarise(Indicador = sum(Ingresos)) %>% 
        ungroup()
    }
  }
  
  return(D)
}
