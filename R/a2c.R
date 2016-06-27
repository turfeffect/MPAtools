#' Convert data format
#'
#' @description Convert fish transect data from format a to c
#'
#' @param a An object of class data.frame with the format a (one column for each size interval).
#'
#' @return c An object of class data.frame with the format c (one column for size and one row for each occurrence).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

a2c=function(a){

  b=a2b(a) #we use the pre-existing function a2b to convert a format to b format
  c=b2c(b) #We use the pre-existing function b2c to convert b format to c format

  return(c) #Return a data.frame

}
