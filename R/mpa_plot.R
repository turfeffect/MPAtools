#' mpa_plot Plots results of mpa_sim
#'
#' @seealso mpa_sim
#'
#'

library(ggplot2)

mpa_plot=function(results, type="io", scenarios=1,
                  in.col="blue", in.type="b",
                  out.col="red", out.type="b",
                  pop.col="black", pop.type="b",
                  catch.col="magenta", catch.type="b",
                  other=NULL,
                  ...){
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
    # We add population outside the MPA
    lines(results$time.series$time, results$time.series$pop.out, col=out.col, type=out.type)
  }

  if (type=="c"){
    plot(results$time.series$time,
         results$time.series$total.catches,
         col=in.col,
         type=in.type,
         xlab="Timesteps",
         ylab="Total Catches (Org)")
  }

  if (type=="p"){
    plot(results$time.series$time,
         results$time.series$pop,
         col=in.col,
         type=in.type,
         xlab="Timesteps",
         ylab="Total Population")
  }

  if (type=="pt"){
    windows()
    for(i in c(1:dim(results$pop)[3])){
      filled.contour(results$pop[,,i])
    }
  }


  # Compare between scenarios
}
