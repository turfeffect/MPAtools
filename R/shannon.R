#' Shannon-Wiener Diversity Index
#'
#' @param data A data.frame
#' @param location A string
#'
#' @return H A data.frame
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
    summarize(N = sum(Abundancia)) %>%
    ungroup() %>%
    mutate(ID = paste(Ano, Zonificacion, Sitio, Transecto)) %>%
    select(ID, N)

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
    summarize(H = -1*sum(pi*log2(pi))) %>%
    ungroup()
}
