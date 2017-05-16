#' Convert data format
#'
#' @description Convert fish transect data from format c to format a; See details. Convierte datos de transectos de peces del formato c al formato a; Ver detalles
#'
#' @param a A data.frame with the format c. Un data.frame en formato c
#'
#' @return A data.frame wih the format a. Un data.frame en formato a
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
#' a <- c2a(c)
#' a
#'
#' @importFrom magrittr %>%
#'

c2a <- function(c){

  a <- c2b(c) %>%  #We can just use the pre-existing functios for this. Convert c format to b format using c2b()
    b2a()          #And now convert b format to a format using b2a().

  return(a) #return a data.frame
}
