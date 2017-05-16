#' Convert data format
#'
#' @description Convert fish transect data from format b to format c; See details. Convierte datos de transectos de peces del formato b al formato c; Ver detalles
#'
#' @param a A data.frame with the format b. Un data.frame en formato b
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
#' #load a "b" sample dataset
#' data("b")
#'
#' #convert a to b format
#' c <- b2c(b)
#' c
#'
#' @importFrom magrittr %>%
#'

b2c <- function(b){

  #Set proper names to cells in Talla based on the size indicated by PromedioDeTalla

  b$Talla[b$PromedioDeTalla<=5] <- "0a5"
  b$Talla[b$PromedioDeTalla>5] <- "6a10"
  b$Talla[b$PromedioDeTalla>10] <- "11a20"
  b$Talla[b$PromedioDeTalla>20] <- "21a30"
  b$Talla[b$PromedioDeTalla>30] <- "31a40"
  b$Talla[b$PromedioDeTalla>40] <- ">40"

  c <- untable(df = b, num = b$Abundancia) #Untable b to convert to c

  c$Abundancia <- 1                    #Set abundances = to one

  return(c) #return a data.frame

}
