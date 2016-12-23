#' Shannon-Wiener Diversity Index
#'
#' @param data A data.frame
#' @param location A string
#'
#' @return H A data.frame
#'
#' @export
#'
#'

shannon <- function(data, location){
  library(tidyr)
  library(dplyr)

  N <- data %>%
    filter(Comunidad == location) %>%
    filter(Abundancia > 0) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto) %>%
    summarize(N = sum(Abundancia),
              Temperatura = mean(Temperatura, na.rm = T),
              Visibilidad = mean(Visibilidad, na.rm = T),
              Profundidad = mean(ProfundidadInicial, na.rm = T)) %>%
    ungroup() %>%
    mutate(ID = paste(Ano, Zonificacion, Sitio, Transecto)) %>%
    select(ID, N, Temperatura, Visibilidad, Profundidad)

  H <- data %>%
    filter(Comunidad == location) %>%
    filter(Abundancia > 0) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto,
             GeneroEspecie) %>%
    summarize(ni = sum(Abundancia)) %>%
    ungroup() %>%
    mutate(ID = paste(Ano, Zonificacion, Sitio, Transecto)) %>%
    left_join(N, by = "ID") %>%
    mutate(pi = ni/N) %>%
    group_by(Ano,
             Zonificacion,
             Sitio,
             Transecto) %>%
    summarize(H = -1*sum(pi*log2(pi)),
              Temperatura = mean(Temperatura, na.rm = T),
              Visibilidad = mean(Visibilidad, na.rm = T),
              Profundidad = mean(Profundidad, na.rm = T)) %>%
    ungroup()

  return(as.data.frame(H))
}
