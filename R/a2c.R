#' Convert data format
#'
#' @description Convert fish transect data from format a to format c; See details. Convierte datos de transectos de peces del formato a al formato c; Ver detalles
#'
#' @param a A data.frame with the format a. Un data.frame en formato a
#'
#' @return A data.frame wih the format c. Un data.frame en formato c
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <jvillasenor@turfeffect.org>
#'
#' @examples
#' #library(MPAtools)
#'
#' #load an "a" sample dataset
#' data("a")
#'
#' #convert a to b format
#' c <- a2c(a)
#' c
#'
#' @importFrom magrittr %>%

a2c <- function(a){

  c <- a2b(a) %>%
    b2c()

  return(c) #Return a data.frame

}
