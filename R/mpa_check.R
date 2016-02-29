#' mpa_sim input validator
#'
#' @description Validates the inputs passed to mpa_2D().
#'
#' @param area an N by M matrix with harvesting rates for fishing grounds (0 < harvesting rate <= 1) and MPAs (harvesting rate = 0) for the simulated area.
#' @param nsteps Number of steps to run the simulation.
#' @param r Intrinsic population growth rate.
#' @param pop0 Initial population size. If single, assumes equal population size across all cells in area. If a matrix, it must have the same dimensions as area.
#' @param K Carrying capacity
#' @param mrate Movement rate expressed as a proportion (relative to 1). Indicates the proportion of organisms that will move out of a cell in one timestep.
#' @param op A logical value that indicates if a filled.contour is to be created. WARNING: op = TRUE may make the simulation slow.
#'
#' @return check an object of type list containing elemetns result and message. Result is a logical that indicates if the inputs to mpa_2d() are correct. If they are not, message contains the error message to be displayed in the Console.
#'
#' @author Villasenor-Derbez, J.C.
#'
#'
#' @export

mpa_check=function(area, nsteps, r, pop0, K, mrate){

  # Checking that area has no values larger than 1 or smaller than 0
  area.dim=dim(area) #To be used later, when checking if dim(pop0)==dim(area) when pop0 is not a single value
  area.check=ifelse(!any(area>1),1,0)
  if (area.check==1){
    area.check=ifelse(!any(area<0),1,0)
  }

  # Checking that nsteps is a single element and is larger than 0
  nsteps.check=ifelse(length(nsteps)==1,1,0)
  if (nsteps.check==1){
    nsteps.check=ifelse(nsteps>0,1,0)
  }

  # Checking that r is a single element
  r.check=ifelse(length(r)==1,1,0)

  # Checking that pop0 is either a single element or a matrix of size dim(area)
  pop0.check=ifelse(is.matrix(pop0),1,0)
  if (pop0.check==1){
    pop0.dim=dim(pop0)
    pop0.check=ifelse(pop0.dim[1]==area.dim[1],1,0)
    if (pop0.check==1){
      pop0.dim=ifelse(pop0.dim[2]==area.dim[2],1,0)
    }
  }
  if (pop0.check==0){
    pop0.check=ifelse(length(pop0)==1,1,0)
  }
      #Checking that pop0 does not have elemts larger than K
  if (pop0.check==1){
    pop0.check=ifelse(any(pop0>K),0,1)
  }

  # Checking that K is a single element
  K.check=ifelse(length(K)==1,1,0)

  # Checking that mrate is a single element
  mrate.check=ifelse(length(mrate)==1,1,0)

  #concatenating all results into a single vector. All must be 1 to continue.

  checks=c(area.check, nsteps.check, r.check, pop0.check, K.check)

  result=ifelse(any(checks==0),0,1)

  message=ifelse(result==0,"One of the inputs has a different size from expected or contains ilegal values (i.e. pop0>K). Use ?mpa_2D or help(mpa_2D) to read more on how to use this function.","")

  check=list(result=result,message=message)

  return(check)
}
