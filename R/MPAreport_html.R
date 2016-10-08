#' Create report for an MPA
#'
#' @description Produces an html document with the analysis of 5 biophysical indicators performed for the given reserve/control site pair
#'
#' @param peces A data.frame
#' @param invertebrados A data.frame
#' @param comunidad A string
#' @param reserva A string
#' @param control A string
#'
#' @export

MPAreport_html <- function(peces, invertebrados, pesca, gov, coop, comunidad, reserva, control) {

  library(rmarkdown)


  title <- paste("Analisis para", reserva, "en la comunidad pesquera", comunidad, sep = " ")

  render(input = system.file("rmarkdown/templates/report/Reporte/Reporte.Rmd", package="MPAtools"),
         params = list(title = title,
                       peces = peces,
                       invertebrados = invertebrados,
                       pesca = pesca,
                       gov = gov,
                       coop = coop,
                       comunidad = comunidad,
                       reserva = reserva,
                       control = control
                       ),
         output_file = paste('/Reporte', comunidad, reserva, '.html', sep=''),
         output_dir = getwd()
  )
}
