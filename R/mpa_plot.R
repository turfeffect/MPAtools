#' Plots results of mpa_sim
#'
#' @description Plots results of mpa_sim
#'
#' @seealso mpa_sim
#' @seealso mpa_compare
#'
#' @export
#'
#' @author Villasenor-Derbez, J.C.

# agregar leyendas a cada caso
# Cambiar scenarios=X a algo mas descriptivbo (one or many)

mpa_plot=function(data, type="io",
                  in.col="blue", in.type="b",
                  out.col="red", out.type="b",
                  pop.col="black", pop.type="b",
                  catch.col="magenta", catch.type="b",
                  other=NULL,
                  ...){

  library(ggplot2)
  library(dplyr)
  library(tidyr)

  results=data
  multiple=is.data.frame(results)

  if (multiple==F){

    #Comprobar que sólo tenga un escenario

    # Inside vs Outside
    if (type=="io"){
      #We plot time vs population inside MPA
      plot(results$time.series$time,
           results$time.series$pop.in,
           col=in.col,
           type=in.type,
           xlab="Timesteps",
           ylab="Population (Org/patch)",
           ylim=c(min(results$time.series$pop.out),
                  max(results$time.series$pop.in)))
      # We add a line for population outside the MPA
      lines(results$time.series$time, results$time.series$pop.out, col=out.col, type=out.type)
    }

    # Total catches through time
    if (type=="c"){
      plot(results$time.series$time,
           results$time.series$catches,
           col=in.col,
           type=in.type,
           xlab="Timesteps",
           ylab="Total Catches (Org)")
    }

    # Total population through time
    if (type=="p"){
      plot(results$time.series$time,
           results$time.series$pop,
           col=in.col,
           type=in.type,
           xlab="Timesteps",
           ylab="Total Pop (Org/patch)")
    }

    # Spatial representation of the population
    if (type=="pt"){
      windows()
      for(i in c(1:dim(results$pop)[3])){
        filled.contour(results$pop[,,i])
      }
    }
  }

  if (multiple==T){

    if (type=="io"){
      #We select the columns of interest and gather only information for inside and outside the reserve.
      io=results%>%
        select(Time, In=Pop.in, Out=Pop.out, Scenario)%>%
        gather(Zone,Pop,-c(1,4))

      #We plot it with ggplot2
      p=ggplot(io, aes(Time,Pop, factor=Zone))+
        geom_point(aes(pch=Zone, col=Scenario))+
        geom_line(aes(col=Scenario))+
        theme_bw()+
        labs(x="Time", y="Population (Org/patch)")
    }

    if (type=="c"){
      p=ggplot(results, aes(Time, Catches, factor=Scenario))+
        geom_point(aes(col=Scenario))+
        geom_line(aes(col=Scenario))+
        theme_bw()

    }

    if (type=="p"){
      p=ggplot(results, aes(Time, Pop, factor=Scenario))+
        geom_point(aes(col=Scenario))+
        geom_line(aes(col=Scenario))+
        theme_bw()+
        labs(x="Time", y="Population (Org/patch)")

    }
    p
  }
}
