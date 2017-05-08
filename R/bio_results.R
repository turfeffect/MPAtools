#' Results
#'
#' @description Analyzes all the required biophysical indicators at once
#'
#' @param data The data to be analyzed
#' @param res The name of the reserve
#' @param con The name of the control site
#' @param values A list, created within the Shiny app, that contains the list of indicators selected and the community
#'
#' @return results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
#' @export
#'
#'

bio_results <- function(values, data, res, con) {
  library(broom)
  # Establish a dataframe where to store all values
  results <- tibble::tibble(
    Ind = c("Shannon", "Riqueza", "OrganismosLT50", "Densidad", "DensidadObj", "NivelTrofico", "Biomasa", "BiomasaObj"),
    e = NA,
    p = NA,
    string = NA,
    color = NA,
    model = list(NA),
    plot = list(NA)
  )

  ####
  if ("Indice de diversidad de Shannon" %in% values$indB) {
    model <- shannon(data, values$comunidad) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[1] <- TidyModel$estimate
    results$p[1] <- TidyModel$p.value
    results$string[1] <- valueBoxString(TidyModel)
    results$color[1] <- bio_score(TidyModel)
    results$model[[1]] <- model
    results$plot[[1]] <- mpa_plot4(model, y.lab = "Shannon (H'/Transecto)")
  }

  ####
  if ("Riqueza" %in% values$indB) {
    model <- richness(data, values$comunidad) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[2] <- TidyModel$estimate
    results$p[2] <- TidyModel$p.value
    results$string[2] <- valueBoxString(TidyModel)
    results$color[2] <- bio_score(TidyModel)
    results$model[[2]] <- model
    results$plot[[2]] <- mpa_plot4(model, y.lab = "Riqueza (Especies/Transecto)")
  }

  ####
  if ("Organismos > LT_50" %in% values$indB) {
    model <- fish_size(data, values$comunidad, values$objsp) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[3] <- TidyModel$estimate
    results$p[3] <- TidyModel$p.value
    results$string[3] <- valueBoxString(TidyModel)
    results$color[3] <- bio_score(TidyModel)
    results$model[[3]] <- model
    results$plot[[3]] <- mpa_plot4(model, y.lab = "Densidad relativa\n(Org > LT_50/TotOrg Transecto)")
  }

  ####
  if ("Densidad" %in% values$indB) {
    model <- density(data, values$comunidad) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[4] <- TidyModel$estimate
    results$p[4] <- TidyModel$p.value
    results$string[4] <- valueBoxString(TidyModel)
    results$color[4] <- bio_score(TidyModel)
    results$model[[4]] <- model
    results$plot[[4]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }

  ####
  if ("Densidad de especies objetivo" %in% values$indB) {
    model <- density(data = data, location = values$comunidad, species = values$objsp) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[5] <- TidyModel$estimate
    results$p[5] <- TidyModel$p.value
    results$string[5] <- valueBoxString(TidyModel)
    results$color[5] <- bio_score(TidyModel)
    results$model[[5]] <- model
    results$plot[[5]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }

    ####
    if ("Nivel trofico" %in% values$indB) {
      model <- trophic(data, values$comunidad) %>%
        turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

      TidyModel <- model$TidyModel %>%
        filter(term == "ZonaReserva:Post1")

      model <- model$model

      results$e[6] <- TidyModel$estimate
      results$p[6] <- TidyModel$p.value
      results$string[6] <- valueBoxString(TidyModel)
      results$color[6] <- bio_score(TidyModel)
      results$model[[6]] <- model
  results$plot[[6]] <- mpa_plot4(model, y.lab = "Nivel trofico")
    }

  ####
  if ("Biomasa" %in% values$indB) {
    model <- fish_biomass(data, values$comunidad) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[7] <- TidyModel$estimate
    results$p[7] <- TidyModel$p.value
    results$string[7] <- valueBoxString(TidyModel)
    results$color[7] <- bio_score(TidyModel)
    results$model[[7]] <- model
  results$plot[[7]] <- mpa_plot4(model, y.lab = "Biomasa (Kg/Transecto)")
  }

  ####
  if ("Biomasa de especies objetivo" %in% values$indB) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[8] <- TidyModel$estimate
    results$p[8] <- TidyModel$p.value
    results$string[8] <- valueBoxString(TidyModel)
    results$color[8] <- bio_score(TidyModel)
    results$model[[8]] <- model
    results$plot[[8]] <- mpa_plot4(model, y.lab = "Biomasa (Kg/Transecto)")
  }

  return(results)
}
