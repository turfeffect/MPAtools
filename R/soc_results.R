#' Results
#'
#' @description Analyzes all the required biophysical indicators at once
#'
#' @param data The data to be analyzed
#' @param values A list, created within the Shiny app, that contains the list of indicators selected and the community
#'
#' @return results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
#' @export
#'
#'

soc_results <- function(values, data) {
  library(broom)
  # Establish a dataframe where to store all values
  results <- tibble::tibble(
    Ind = c("Arribos", "Ingresos"),
    e = NA,
    p = NA,
    string = NA,
    color = NA,
    model = list(NA),
    plot = list(NA)
  )

  #### For Landings
  if ("Arribos" %in% values$indS) {
    model <- landings(data, values$comunidad, "kg") %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")

    model <- model$model

    results$e[1] <- TidyModel$estimate
    results$p[1] <- TidyModel$p.value
    results$string[1] <- valueBoxString(TidyModel)
    results$color[1] <- soc_score(TidyModel)
    results$model[[1]] <- model
    results$plot[[1]] <- soc_plot(model, y.lab = "Arribos (Kg / Ano)")
  }

  #### For Income
  if ("Ingresos por arribos" %in% values$indS) {
    model <- landings(data, values$comunidad, "price") %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")

    model <- model$model

    results$e[2] <- TidyModel$estimate
    results$p[2] <- TidyModel$p.value
    results$string[2] <- valueBoxString(TidyModel)
    results$color[2] <- soc_score(TidyModel)
    results$model[[2]] <- model
    results$plot[[2]] <- soc_plot(model, y.lab = "Ingresos por arribos (Pesos / Ano)")
  }

  return(results)
}
