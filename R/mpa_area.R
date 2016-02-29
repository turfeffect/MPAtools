#' area matrix generator
#'
#' @description Generates the area matrix that will be simulated, based on size of the area, size of the MPA, number of MPAs,harvesting rate, and harvesting strategy.
#'
#' @param m number of vertical cells (rows)
#' @param n number of horizontal cells (columns)
#' @param mpa.size proportion of cells to be MPAs (from 0 to 1)
#' @param mpa.number number of MPAs
#' @param harvesting.rate harvesting rate expressed as a proportion (0 to 1) of organisms harvested per cell. If harvesting.strategy = TRUE, this is the maximum value observed in the borders of the MPA.
#' @param harvesting.strategy A logical value that indicates if "Fising the line" occurs. harvesting.strategy=FALSE is the default that indicates equal harvesting rate outside the MPA. If TRUE, it creates a pattern in which harvesting rate depends on the distance to MPA.
#'
#' @return area A matriz of size m by n that simulates harvesting rates and MPAs
#'
#' @author Villasenor-Derbez, J.C.
#'
#'
#' @export
#'

mpa_area=function(m,n,mpa.size,mpa.number,harvesting.rate=.1,harvesting.strategy=FALSE){
  area=matrix(harvesting.rate,nrow=m,ncol=n)
  mpa=n*m*mpa.size

  area[]=0

}











