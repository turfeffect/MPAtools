#' Convert data format
#'
#' @description Convert fish transect data from format c to format b; See details. Convierte datos de transectos de peces del formato c al formato b; Ver detalles
#'
#' @param a A data.frame with the format c. Un data.frame en formato c
#'
#' @return A data.frame wih the format b. Un data.frame en formato b
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <jvillasenor@turfeffect.org>
#'
#' @examples
#' #library(MPAtools)
#'
#' #load an "c" sample dataset
#' data("c")
#'
#' #convert c to a format
#' b <- c2b(c)
#' b
#'
#' @importFrom magrittr %>%
#'

c2b <- function(c){

  b <- c %>% #Set b equal to c
    group_by(Dia, Mes, Ano, Estado, Comunidad, Sitio, Latitud, Longitud, Habitat, Zonificacion, TipoDeProteccion, ANP, BuzoMonitor, HoraInicialBuceo, HoraFinalBuceo, ProfundidadInicial, ProfundidadFinal, Temperatura, Visibilidad, Corriente, Transecto, Genero, Especie, GeneroEspecie, Sexo, Talla, PromedioDeTalla) %>%
    dplyr::summarize(Abundancia=sum(Abundancia, na.rm = T))

  return(b) #return a data.frame

}
