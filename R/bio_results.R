#' Biophysical Results
#'
#' @description Analyzes all the required biophysical indicators at once
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

bio_results <- function(values, data, res, con) {
  library(broom)
  # Establish a dataframe where to store all values
  results <- tibble::tibble(
    Ind = c("Shannon", "Riqueza", "LT50Obj1", "Densidad", "DensidadObj1", "NivelTrofico", "Biomasa", "BiomasaObj1", "LT50Obj2", "LT50Obj3", "LT50Obj4", "LT50Obj5", "LT50Obj6", "LT50Obj7", "LT50Obj8", "BiomasaObj2", "BiomasaObj3", "BiomasaObj4", "BiomasaObj5", "BiomasaObj6", "BiomasaObj7", "BiomasaObj8", "DensidadObj2", "DensidadObj3", "DensidadObj4", "DensidadObj5", "DensidadObj6", "DensidadObj7", "DensidadObj8"),
    e = NA,
    p = NA,
    string = NA,
    color = NA,
    model = list(NA),
    plot = list(NA)
  )
  
  ########################################################
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

  #########################################################
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

  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 0) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[1]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[3] <- TidyModel$estimate
    results$p[3] <- TidyModel$p.value
    results$string[3] <- valueBoxString(TidyModel)
    results$color[3] <- bio_score(TidyModel)
    results$model[[3]] <- model
    results$plot[[3]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[1], "\n(Org > LT_50/TotOrg Transecto)"))
  }

  #########################################################
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

  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 0) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[1]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[5] <- TidyModel$estimate
    results$p[5] <- TidyModel$p.value
    results$string[5] <- valueBoxString(TidyModel)
    results$color[5] <- bio_score(TidyModel)
    results$model[[5]] <- model
    results$plot[[5]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[1], "\n(Organismos/Transecto)"))
  }

    #########################################################
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

  #########################################################
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

  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 0) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[1]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)

    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")

    model <- model$model

    results$e[8] <- TidyModel$estimate
    results$p[8] <- TidyModel$p.value
    results$string[8] <- valueBoxString(TidyModel)
    results$color[8] <- bio_score(TidyModel)
    results$model[[8]] <- model
    results$plot[[8]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[1], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 1) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[2]) %>%
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
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[2], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 2) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[3]) %>%
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
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[3], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 3) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[4]) %>%
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
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[4], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 4) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[5]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 12
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[5], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 5) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[6]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 13
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[6], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 6) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[7]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 14
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[7], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Organismos > LT_50" %in% values$indB & length(values$objsp$sp) > 7) {
    model <- fish_size(data, values$comunidad, values$objsp$sp[8]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 15
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad relativa",values$objsp$sp[8], "\n(Org > LT_50/TotOrg Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 1) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[2]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 16
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[2], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 2) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[3]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 17
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[3], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 3) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[4]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 18
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[4], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 4) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[5]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 19
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[5], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 5) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[6]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 20
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[6], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 6) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[7]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 21
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[7], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Biomasa de especies objetivo" %in% values$indB & length(values$objsp$sp) > 7) {
    model <- fish_biomass(data = data, location = values$comunidad, species = values$objsp$sp[8]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 22
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Biomasa",values$objsp$sp[8], "\n(Kg/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 1) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[2]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 23
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[2], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 2) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[3]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 24
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model,y.lab = paste("Densidad",values$objsp$sp[3], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 3) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[4]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 25
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[4], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 4) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[5]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 26
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[5], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 5) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[6]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 27
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[6], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 6) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[7]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 28
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[7], "\n(Organismos/Transecto)"))
  }
  
  #########################################################
  if ("Densidad de especies objetivo" %in% values$indB & length(values$objsp$sp) > 7) {
    model <- density(data = data, location = values$comunidad, species = values$objsp$sp[8]) %>%
      turfeffect(res, con, type = "bio", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "ZonaReserva:Post1")
    
    model <- model$model
    
    n_obj <- 29
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- bio_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- mpa_plot4(model, y.lab = paste("Densidad",values$objsp$sp[8], "\n(Organismos/Transecto)"))
  }

  return(results)
}
