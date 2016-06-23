#' Convert data format
#'
#' @description Convert fish transect data from format a to c
#'
#' @param b An object of class data.frame with the format a (one column for each size interval).
#'
#' @return c An object of class data.frame with the format c (one column for size and one row for each occurrence).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

b2c=function(b){

  library(dplyr)  # Load dplyr
  library(tidyr)  # Load tidyr
  library(reshape)

  c=untable(df=b, num=b$Abundancia)

  c$Abundancia=1


  return(c)

}
