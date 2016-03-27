#' mpa_compare
#'
#' @description Creates a list containing results from multiple scenarios ran in mpa_sim
#'
#' @seealso mpa_plot
#' @seealso mpa_sim
#'


# Esta mierda junta más de un resultado y se puede pasar a mpa_plot para comparar múltiples scenarios

# Los inputs son una lista con forma results=list(results,
                                                #results,
                                                #results...)


mpa_compare=function(results, names){

  nscenarios=length(results)
  Time=0
  Pop.in=0
  Pop.out=0
  Pop=0
  Catches=0
  Scenario=c("")

  #A for loop concatenates all scenarios into a single column for each variable

  for (i in c(1:nscenarios)){
    scenario_i=results[[i]][[1]]

    #The following two if statements evaluate if p0 must be i or i+1. This helps the concatenation.
    if (i==1){
      p0=length(Time)
    }
    if (i>1){
      p0=length(Time)+1
    }

    #p0 (above) indicates starting position to start putting together the vectors. p1 indicates the last position.
    p1=p0+length(scenario_i$time)-1

    #time
    Time[p0:p1]=scenario_i$time

    #pop.in
    Pop.in[p0:p1]=scenario_i$pop.in

    #pop.out
    Pop.out[p0:p1]=scenario_i$pop.out

    #pop
    Pop[p0:p1]=scenario_i$pop

    #catches
    Catches[p0:p1]=scenario_i$catches

    #scenarios
    Scenario[p0:p1]=names[i]

  }

  #Results are exported as a data.frame
  Results=data.frame(Time, Pop.in, Pop.out, Pop, Catches, Scenario)


return(Results)
}
