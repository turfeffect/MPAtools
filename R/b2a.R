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

  library(dplyr) #Load dplyr
  library(tidyr) #Load tidyr

  b$Talla[b$PromedioDeTalla<=5]="0a5"
  b$Talla[b$PromedioDeTalla>5]="6a10"
  b$Talla[b$PromedioDeTalla>10]="11a20"
  b$Talla[b$PromedioDeTalla>20]="21a30"
  b$Talla[b$PromedioDeTalla>30]="31a40"
  b$Talla[b$PromedioDeTalla>40]=">40"

  a=b%>%                      #Set a equal to b
    spread(Talla, Abundancia) #Spread the values in Talla as indicated by Abundance

  a$Talla=a$PromedioDeTalla #Set Talla equal to PromedioDeTalla, because we have lost Talla in spread. Talla now contains sizes of fish >40 cm in this weird format (line38)

  a=select(a, -26) #Select all columns except for column 26 (PromedioDeTalla)


  a[is.na(a)]=0 #set NA values = 0 so that we can sum properly later on

  a$Total=a$'0a5'+a$'6a10'+a$'11a20'+a$'21a30'+a$'31a40'+a$'>40' #Create a column of total with abundances for each spp

  a$Talla[a$Talla<41]=NA #Set values of Talla that are smaller than 41 to NA

  a[a==0]=NA #Set values of 0 back to NA

  a=select(a, c(1:25, 27, 31, 28:30, 26, 32, 33)) #Reorder the columns

  return(a) #return the data.frame

}
