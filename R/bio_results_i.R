#' Biophysical Results
#'
#' @description Analyzes all the required biophysical indicators (for invertebrates) at once
#'
#' @param values A list, created within the Shiny app, that contains the list of indicators selected, the community, any objective species, and the year of implementation of the reserve
#' @param data A data.frame containing the raw data to be analyzed
#' @param res A string specifying the name of the reserve site(s)
#' @param con A string specifying the name of the control site(s)
#'
#' @return results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
#' @export
#'
#' @author Villasenor-Derbez, J.C. <jvillasenor@turfeffect.org>
#'
#' @importFrom magrittr %>%

bio_results_i <- function(values, data, res, con) {
  library(broom)
  # Establish a dataframe where to store all values
  results <- tibble::tibble(
    Ind = c("Shannon", "Riqueza", "Densidad", "DensidadObj1", "DensidadObj2", "DensidadObj3", "DensidadObj4", "DensidadObj5", "DensidadObj6", "DensidadObj7", "DensidadObj8"),
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
  if ("Densidad" %in% values$indB) {
    model <- density(data, values$comunidad) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[3] <- TidyModel$estimate
    results$p[3] <- TidyModel$p.value
    results$string[3] <- valueBoxString(TidyModel)
    results$color[3] <- bio_score(TidyModel)
    results$model[[3]] <- model
    results$plot[[3]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 0) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[1]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 4
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 1) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[2]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 5
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 2) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[3]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 6
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 3) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[4]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 7
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 4) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[5]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 8
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 5) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[6]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 9
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 6) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[7]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 10
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }
  
  
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 7) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[8]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 11
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = "Densidad (Organismos/Transecto)")
  }

  return(results)
}
