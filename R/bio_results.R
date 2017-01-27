#' Results
#'
#' @description Analyzes all the required indicators at once
#'
#' @param input The input list from a Shiny application
#' @param data The data to be analyzed
#' @param res The name of the reserve
#' @param con The name of the control site
#'
#' @return results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
#' @export
#'
#'

bio_results <- function(values, data, res, con) {
  library(broom)
  # Establish a dataframe where to store all values
  results <- tibble::tibble(
    Ind = c(
      "Shannon",
      "Riqueza",
      "OrganismosLT50",
      "Densidad",
      "DensidadObj",
      "NivelTrofico",
      "Biomasa",
      "BiomasaObj"
    ),
    e = NA,
    p = NA,
    string = NA,
    color = NA,
    model = list(NA)
  )

  ####
  if ("Índice de diversidad de Shannon" %in% values$indB) {
    model <- MPAtools::shannon(data, values$comunidad) %>%
      turfeffect(res, con)

    TidyModel <- tidy(model) %>%
      filter(term == "Ano:Zona")

    results$e <- TidyModel$estimate
    results$p <- TidyModel$p.value
    results$string <- valueBoxString(model)
    results$color <- score(model)
    results$model[[1]] <- model
  }

  ####
  if ("Riqueza" %in% values$indB) {
    model <- richness(data, values$comunidad) %>%
      turfeffect(res, con)

    TidyModel <- tidy(model) %>%
      filter(term == "Ano:Zona")

    results$e <- TidyModel$estimate
    results$p <- TidyModel$p.value
    results$string <- valueBoxString(model)
    results$color <- score(model)
    results$model[[2]] <- model
  }

  ####
  # if ("Organismos > LT_50" %in% input$indB) {
  #   model <- shannon(datasetInput(), input$comunidad, res.fun(), con.fun()) %>%
  #     turfeffect()
  #
  #   TidyModel <- tidy(model) %>%
  #     filter(term == "Ano:Zona")
  #
  #   results$e <- TidyModel$estimate
  #   results$p <- TidyModel$p.value
  #   results$string <- valueBoxString(model)
  #   results$color <- score(model)
  #   results$model[[3]] <- model
  # }

  ####
  if ("Densidad" %in% values$indB) {
    model <- density(data, values$comunidad) %>%
      turfeffect(res, con)

    TidyModel <- tidy(model) %>%
      filter(term == "Ano:Zona")

    results$e <- TidyModel$estimate
    results$p <- TidyModel$p.value
    results$string <- valueBoxString(model)
    results$color <- score(model)
    results$model[[4]] <- model
  }

  ####
  # if ("Densidad de especies objetivo" %in% input$indB) {
  #   model <- shannon(datasetInput(), input$comunidad, res.fun(), con.fun()) %>%
  #     turfeffect()
  #
  #   TidyModel <- tidy(model) %>%
  #     filter(term == "Ano:Zona")
  #
  #   results$e <- TidyModel$estimate
  #   results$p <- TidyModel$p.value
  #   results$string <- valueBoxString(model)
  #   results$color <- score(model)
  #   results$model[[5]] <- model
  # }
#
#   ####
#   if ("Nivel trófico" %in% values$indB) {
#     model <- trophic(data, values$comunidad) %>%
#       turfeffect(res, con)
#
#     TidyModel <- tidy(model) %>%
#       filter(term == "Ano:Zona")
#
#     results$e <- TidyModel$estimate
#     results$p <- TidyModel$p.value
#     results$string <- valueBoxString(model)
#     results$color <- score(model)
#     results$model[[6]] <- model
#   }

  # ####
  # if ("Biomasa" %in% values$indB) {
  #   model <- fish_biomass(data, values$comunidad) %>%
  #     turfeffect(res, con)
  #
  #   TidyModel <- tidy(model) %>%
  #     filter(term == "Ano:Zona")
  #
  #   results$e <- TidyModel$estimate
  #   results$p <- TidyModel$p.value
  #   results$string <- valueBoxString(model)
  #   results$color <- score(model)
  #   results$model[[7]] <- model
  # }

  #   ####
  #   if ("Biomasa de especies objetivo" %in% input$indB) {
  #     model <- shannon(datasetInput(), input$comunidad, res.fun(), con.fun()) %>%
  #       turfeffect()
  #
  #     TidyModel <- tidy(model) %>%
  #       filter(term == "Ano:Zona")
  #
  #     results$e <- TidyModel$estimate
  #     results$p <- TidyModel$p.value
  #     results$string <- valueBoxString(model)
  #     results$color <- score(model)
  #     results$model[[8]] <- model
  #   }

  return(results)
}
