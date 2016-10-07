MPA_summary <- function(peces, invertebrados, comunidad, reserva, control){

  library(rmarkdown)

# Para peces

  Dp <- summary(turfeffect(density(peces, comunidad), reserva, control))
  Sp <- summary(turfeffect(shannon(peces, comunidad), reserva, control))
  Bp <- summary(turfeffect(fish_biomass(peces, comunidad), reserva, control))
  NT <- summary(turfeffect(trophic(peces, comunidad), reserva, control))

  Di <- summary(turfeffect(density(invertebrados, comunidad), reserva, control))

  summary <- list(Bio = list(P = list(Dp = score(x = data.frame(est = coefficients(Dp)[3],
                                                       p = coefficients(Dp)[12])),

                                      Sp = score(x = data.frame(est = coefficients(Sp)[3],
                                                                p = coefficients(Sp)[12])),

                                      Bp = score(x = data.frame(est = coefficients(Bp)[3],
                                                                p = coefficients(Bp)[12])),

                                      NT = score(x = data.frame(est = coefficients(NT)[3],
                                                                p = coefficients(NT)[12]))),

                             I = list(Di = score(x = data.frame(est = coefficients(Di)[3],
                                                                p = coefficients(Di)[12]))),

                             O = list(D = 0)),

                  Soc = list(1),

                  Gov = list(1))

  render(input = "inst/rmarkdown/templates/report/Resumen/Resumen.Rmd",
         params = list(summary = summary),
         output_file = paste('/Reporte', comunidad, reserva, '.html', sep=''),
         output_dir = getwd()
         )
}
