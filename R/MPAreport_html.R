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

MPAreport_html <- function(peces, invertebrados, comunidad, reserva, control) {

  library(rmarkdown)

  title <- paste("Análisis para", reserva, "en la comunidad pesquera", comunidad, sep = " ")

  render(input = "ReporteSAM.Rmd",
         params = list(title = title,
                       peces = peces,
                       invertebrados = invertebrados,
                       comunidad = comunidad,
                       reserva = reserva,
                       control = control
         ),
         output_file = paste('/Reporte', comunidad, reserva, '.html', sep=''),
         output_dir = getwd()
  )
}
