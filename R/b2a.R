#' Convert data format
#'
#' @description Convert fish transect data from format b to format a; See details. Convierte datos de transectos de peces del formato b al formato a; Ver detalles
#'
#' @param a A data.frame with the format b. Un data.frame en formato b
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
#' #load a "b" sample dataset
#' data("b")
#'
#' #convert a to b format
#' a <- b2a(b)
#' a
#'
#' @importFrom magrittr %>%
#'

b2a <- function(b){

  b$PromedioDeTalla <- as.numeric(b$PromedioDeTalla)

  b$PromedioDeTalla[is.na(b$PromedioDeTalla)] <- 41

  #Set proper names to cells in Talla based on the size indicated by PromedioDeTalla

  b$Talla[b$PromedioDeTalla<=5] <- "0a5"
  b$Talla[b$PromedioDeTalla>5] <- "6a10"
  b$Talla[b$PromedioDeTalla>10] <- "11a20"
  b$Talla[b$PromedioDeTalla>20] <- "21a30"
  b$Talla[b$PromedioDeTalla>30] <- "31a40"
  b$Talla[b$PromedioDeTalla>40] <- ">40"

  b$rows <- 1:nrow(b)

  a <- b%>%                      #Set a equal to b
    tidyr::spread(Talla, Abundancia) %>% #Spread the values in Talla as indicated by Abundance
    dplyr::select(-rows)

  a$Talla <- a$PromedioDeTalla #Set Talla equal to PromedioDeTalla, because we have lost Talla in spread. Talla now contains sizes of fish >40 cm in this weird format (line38)

  a <- select(a, -PromedioDeTalla) #Select all columns except for column 26 (PromedioDeTalla)


  a[is.na(a)] <- 0 #set NA values = 0 so that we can sum properly later on

  a$Total <- a$'0a5'+a$'6a10'+a$'11a20'+a$'21a30'+a$'31a40'+a$'>40' #Create a column of total with abundances for each spp

  a$Talla[a$Talla<41] <- NA #Set values of Talla that are smaller than 41 to NA

  a[a==0] <- NA #Set values of 0 back to NA

  a <- select(a, c(1:25, 27, 31, 28:30, 26, 32, 33)) #Reorder the columns

  return(a) #return the data.frame

}
