#' Convert data format
#'
#' @description
#'
#' @param c An object of class data.frame wih the format c (One column for sizes and one for abundance).
#'
#' @return a An object of class data.frame with the format a (one column for each size interval).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

c2a=function(c){

  b=c2b(c) #We can just use the pre-existing functios for this. Convert c format to b format using c2b()
  a=b2a(b) #And now convert b format to a format using b2a().

  return(a) #return a data.frame
}
