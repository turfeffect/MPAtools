
check_param=function(area, nsteps, r, pop0, K, mrate){

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
