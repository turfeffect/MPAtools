#############################
# Governance indicators selection function
# Caio Faro
# 12/07/2016
#############################

indG_sel <- function(objective) {

  if (any(objective == "A")){
    gov_sel = c("Acceso a la pesquería",
                "Número de pescadores",
                "Reconocimiento legal de la reserva",
                "Grado de pesca ilegal",
                "Plan de manejo",
                "Tamaño de la reserva",
                "Razonamiento para el diseño de la reserva",
                "Pertenencia a oragnizaciones pesqueras",
                "Tipo de organización pesquera",
                "Representación")
  }

  else {
    gov_sel = c("Acceso a la pesquería",
                "Plan de manejo",
                "Tamaño de la reserva",
                "Razonamiento para el diseño de la reserva",
                "Representación")
  }

  return(gov_sel)

}
