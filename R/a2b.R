#' Convert data format
#'
#' @description Convert fish transect data from format a to format b; See details. Convierte datos de transectos de peces del formato a al formato b; Ver detalles
#'
#' @param a A data.frame with the format a. Un data.frame en formato a
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
#' #load an "a" sample dataset
#' data("a")
#'
#' #convert a to b format
#' b <- a2b(a)
#' b
#'
#' @importFrom magrittr %>%
#'

a2b <- function(a){

  b <- a                         #Set b equal to a
  b$row <- 1:nrow(b)             #Add a column with row number. This is a hack by Hadley, it is then deleted
  b <- b%>%                      #Start piping function
    dplyr::select(-Total) %>%        #Use all columns except for "Total"
    tidyr::spread(Talla, ">40") %>%  #spread the column of fish larger than 40
    dplyr::select(-row) %>%          #delete the column created above (b$row)
    tidyr::gather(Talla, Abundancia, -c(1:25)) %>% #gather sizes into a singe column with respective abundances
    dplyr::filter(Abundancia>0)      #gather includes a bunch of 0's by default, so we filter them out

  #Las líneas de abajo asignan los nombres correctos a las celdas, e inlcuyen los promedios que deben de ser utilizados:

  ## Lo hacemos para la columna Promedio de Talla (la que se usa en el análisis)
  b$PromedioDeTalla <- as.numeric(b$Talla)
  b$PromedioDeTalla[b$Talla=="0a5"] <- 2.5
  b$PromedioDeTalla[b$Talla=="6a10"] <- 8.5
  b$PromedioDeTalla[b$Talla=="11a20"] <- 15.5
  b$PromedioDeTalla[b$Talla=="21a30"] <- 25.5
  b$PromedioDeTalla[b$Talla=="31a40"] <- 35.5

  ## Y lo hacemos para la columna Talla
  b$Talla[b$Talla=="0a5"] <- "0a5"
  b$Talla[b$Talla=="6a10"] <- "6a10"
  b$Talla[b$Talla=="11a20"] <- "11a20"
  b$Talla[b$Talla=="21a30"] <- "21a30"
  b$Talla[b$Talla=="31a40"] <- "31a40"
  b$Talla[b$PromedioDeTalla>=41] <- ">40"

  b <- b %>%
    dplyr::select(c(1:26, 28, 27)) #We re-order columns into the proper order

  return(b) #return the data.frame

}
