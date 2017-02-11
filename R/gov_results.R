#' GOvernance Results
#'
#' @description Analyzes all the required governance indicators at once
#'
#' @param data The data to be analyzed
#' @param reserva The name of the reserve
#' @param values A list, created within the Shiny app, that contains the list of indicators selected and the community
#'
#' @return results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
#' @export
#'

gov_results <- function(values, data, reserva){
  library(tidyverse)

  data <- filter(data, Community == values$comunidad, Q2_Name == reserva)

  pescadores <- select(data, Q10Bef, Q10Aft) %>% list()
  reconocimiento <- select(data, Q5, Q5_1, Q5_2) %>% list()
  pesca_ilegal <- select(data, Q13, Q14_11, Q14_12, Q14_21, Q14_22, Q19Bef, Q19Aft, Q20_1, Q20_2, Q20_3) %>% list()
  procuracion <- select(data, Q4_1, Q4_2, Q4_3, Q8, Q15, Q16, Q17) %>% list()
  representacion = select(data, Q3, Q12, Q12_1) %>% list()

  # input <- tibble(Acceso = data$Q7,
  #                 Pescadores = pescadores,
  #                 Reconocimiento = reconocimiento,
  #                 Tipo = NA,
  #                 Pesca_Ilegal = pesca_ilegal,
  #                 Plan_manejo = data$Q6,
  #                 Procuracion = procuracion,
  #                 Tamano = NA,
  #                 Razonamiento = data$Q2_Reason,
  #                 Org_pesquera = data$Q1,
  #                 Representacion = representacion,
  #                 Reglas_internas = data$Q9,
  #                 Effectividad = data$Q23
  # )

  results <- tibble(Ind = c("Acceso a la pesqueria",
                            "Reconocimiento legal de la reserva",
                            "Grado de pesca ilegal",
                            "Plan de manejo",
                            "Procuracion de la reserva",
                            "Tipo de organizacion pesquera",
                            "Representacion",
                            "Reglamentacion interna",
                            "Efectividad percibida"),
                    e = NA,
                    p = NA,
                    string = NA,
                    color = NA,
                    model = list(NA),
                    plot = list(NA))

  ####
  if ("Acceso a la pesqueria" %in% values$indG) {
    results$e[1] <- 1
    results$string[1] <- c("Bien! Tu concesion promueve la extraccion sustentable de tus recursos.")
    results$color[1] <- c("olive")
  }

  if ("Reconocimiento legal de la reserva" %in% values$indG) {
    results$e[2] <- 0
    results$string[2] <- c("Es bueno que esten en proceso. Reconocer su reserva legalmente es importante para fortalecer su procuracion.")
    results$color[2] <- c("yellow")
  }

  if ("Grado de pesca ilegal" %in% values$indG) {
    results$e[3] <- 1
    results$string[3] <- c("La pesca ilegal suele disminuir las abundancias y biomasas de tus recursos.")
    results$color[3] <- c("green")
  }

  if ("Plan de manejo" %in% values$indG) {
    results$e[4] <- 0
    results$string[4] <- c("Tienes un Estudio Tecnico Justificativo, pero seria mejor tener todas las reglas por escrito en un solo lugar.")
    results$color[4] <- c("yellow")
  }

  if ("Procuracion de la reserva" %in% values$indG) {
    results$e[5] <- 1
    results$string[5] <- c("Bien! La vigilancia comunitaria ayuda a mantener fuera a los piratas.")
    results$color[5] <- c("olive")
  }

  if ("Tipo de organizacion pesquera" %in% values$indG) {
    results$e[6] <- 1
    results$string[6] <- c("Bien! Las cooperativas promueven el dialogo entre pescadores y la extraccion sustentable de tus recursos.")
    results$color[6] <- c("olive")
  }

  if ("Representacion" %in% values$indG) {
    results$e[7] <- 1
    results$string[7] <- c("Bien! Al tener alta representacion durante el diseno dela reserva, te aseguras de que mas gente este enterada del proceso.")
    results$color[7] <- c("olive")
  }

  if ("Reglamentacion interna" %in% values$indG) {
    results$e[8] <- 1
    results$string[8] <- c("Bien! Las reglas internas de tu organizacion pesquera evitan malos comportamientos y sobreexplotacion de recursos.")
    results$color[8] <- c("olive")
  }

  if ("Efectividad percibida" %in% values$indG) {
    results$e[9] <- 1
    results$string[9] <- c("Bien! Es bueno saber que la mayoria considera que la reserva funciona.")
    results$color[9] <- c("olive")
  }


  return(results)
}
