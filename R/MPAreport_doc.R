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

MPAreport_doc <- function(peces, invertebrados, comunidad, reserva, control) {

  library(rmarkdown)

  Dp <- summary(turfeffect(density(peces, comunidad), reserva, control))
  Sp <- summary(turfeffect(richness(peces, comunidad), reserva, control))
  Bp <- summary(turfeffect(fish_biomass(peces, comunidad), reserva, control))
  NT <- summary(turfeffect(trophic(peces, comunidad), reserva, control))

  Di <- summary(turfeffect(density(invertebrados, comunidad), reserva, control))

  lang <- filter(invertebrados, GeneroEspecie == "Panulirus argus")
  Nlang <- summary(turfeffect(density(lang, comunidad), reserva, control))

  car <- filter(invertebrados, GeneroEspecie == "Strombus gigas")
  Ncar <- summary(turfeffect(density(car, comunidad), reserva, control))

  lut <- filter(peces, Genero == "Lutjanus")
  Nlut <- summary(turfeffect(density(lut, comunidad), reserva, control))
  Blut <- summary(turfeffect(fish_biomass(lut, comunidad), reserva, control))

  summary <- list(Bio = list(P = list(Dp = score(x = data.frame(est = coefficients(Dp)[4],
                                                                p = coefficients(Dp)[16])),

                                      Sp = score(x = data.frame(est = coefficients(Sp)[4],
                                                                p = coefficients(Sp)[16])),

                                      Bp = score(x = data.frame(est = coefficients(Bp)[4],
                                                                p = coefficients(Bp)[16])),

                                      NT = score(x = data.frame(est = coefficients(NT)[4],
                                                                p = coefficients(NT)[16]))),

                             I = list(Di = score(x = data.frame(est = coefficients(Di)[4],
                                                                p = coefficients(Di)[16]))),

                             O = list(L = score(x = data.frame(est = coefficients(Nlang)[4],
                                                               p = coefficients(Nlang)[16])),

                                      C = score(x = data.frame(est = coefficients(Ncar)[4],
                                                               p = coefficients(Ncar)[16])),

                                      P = score(x = data.frame(est = coefficients(Nlut)[4],
                                                               p = coefficients(Nlut)[16])),

                                      Pb = score(x = data.frame(est = coefficients(Blut)[4],
                                                                p = coefficients(Blut)[16])))),

                  Soc = list(1),

                  Gov = list(1))

  title <- paste("Analisis para", reserva, "en la comunidad pesquera", comunidad, sep = " ")

  render(input = system.file("rmarkdown/templates/report/Reporte/Reporte_doc.Rmd", package="MPAtools"),
         params = list(title = title,
                       peces = peces,
                       invertebrados = invertebrados,
                       comunidad = comunidad,
                       reserva = reserva,
                       control = control,
                       summary = summary
         ),
         output_file = paste('/Reporte', comunidad, reserva, '.docx', sep=''),
         output_dir = getwd()
  )
}
