MPA_summary <- function(peces, invertebrados, comunidad, reserva, control){

# Para peces

  Dp <- summary(turfeffect(density(peces, comunidad), reserva, control))
  Sp <- summary(turfeffect(richness(peces, comunidad), reserva, control))
  Bp <- summary(turfeffect(fish_biomass(peces, comunidad), reserva, control))
  NT <- summary(turfeffect(trophic(peces, comunidad), reserva, control))

  Di <- summary(turfeffect(density(invertebrados, comunidad), reserva, control))
  Si <- summary(turfeffect(density(invertebrados, comunidad), reserva, control))

  summary <- list(Bio = list(P = list(Dp = list(est = Dp$coefficients[[2]],
                                               p = Dp$coefficients[[11]]),

                                      Sp = list(est = Dp$coefficients[[2]],
                                               p = Sp$coefficients[[11]]),

                                      Bp = list(est = Bp$coefficients[[2]],
                                                p = Bp$coefficients[[11]]),

                                      NT = list(est = NT$coefficients[[2]],
                                                p = NT$coefficients[[11]])),

                             I = list(Di = list(est = Di$coefficients[[2]],
                                               p = Di$coefficients[[11]]),

                                      Si = list(est = Si$coefficients[[2]],
                                               p = Si$coefficients[[11]])),
                             O = list(D = 0)),
                  Soc = list(1),
                  Gov = list(1))

  return(summary)
}
