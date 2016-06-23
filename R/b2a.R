#' Convert data format
#'
#' @description
#'
#' @param b An object of class data.frame wih the format b (One column for sizes and one for abundance).
#'
#' @return a An object of class data.frame with the format a (one column for each size interval).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

b2a=function(b){

  library(dplyr)
  library(tidyr)

  a=b%>%
    spread(Talla, Abundancia)

  a$Talla=a$'Promedio de talla'

  a=select(a, -26)


  a[is.na(a)]=0

  a$Total=a$'0a5'+a$'6a10'+a$'11a20'+a$'21a30'+a$'31a40'+a$'>40'

  a$Talla[a$Talla<41]=NA

  a[a==0]=NA

  a=select(a, c(1:25, 27, 31, 28:30, 26, 32, 33))


  return(a)

}
