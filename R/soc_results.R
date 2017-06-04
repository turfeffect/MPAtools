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
    Ind = c("Arribos", "Ingresos", "ArribosObj1", "ArribosObj2", "ArribosObj3", "ArribosObj4", "ArribosObj5", "ArribosObj6", "ArribosObj7", "ArribosObj8", "IngresosObj1", "IngresosObj2", "IngresosObj3", "IngresosObj4", "IngresosObj5", "IngresosObj6", "IngresosObj7", "IngresosObj8"),
    e = NA,
    p = NA,
    string = NA,
    color = NA,
    model = list(NA),
    plot = list(NA)
  )
  
  #### For Landings
  if ("Arribos" %in% values$indS) {
    model <- landings(data = data, location = values$comunidad, type = "kg") %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    results$e[1] <- TidyModel$estimate
    results$p[1] <- TidyModel$p.value
    results$string[1] <- valueBoxString(TidyModel)
    results$color[1] <- soc_score(TidyModel)
    results$model[[1]] <- model
    results$plot[[1]] <- soc_plot(soc_data, y.lab = "Arribos (Kg/Ano)")
  }
  
  #### For Income
  if ("Ingresos por arribos" %in% values$indS) {
    model <- landings(data = data, location = values$comunidad, type = "price") %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    results$e[2] <- TidyModel$estimate
    results$p[2] <- TidyModel$p.value
    results$string[2] <- valueBoxString(TidyModel)
    results$color[2] <- soc_score(TidyModel)
    results$model[[2]] <- model
    results$plot[[2]] <- soc_plot(soc_data, y.lab = "Ingresos por arribos (Pesos/Ano)")
  }
  
  #### Objective species
  
  available_sp <- unique(data$GeneroEspecie)
  
  #### Landings 1
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 0 & values$objsp$sp[1] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[1]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 3
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[1],"(Kg/Ano)"))
  }
  
  #### Landings 2
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 1 & values$objsp$sp[2] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[2]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 4
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[2],"(Kg/Ano)"))
  }
  
  #### Landings 3
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 2 & values$objsp$sp[3] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[3]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 5
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[3],"(Kg/Ano)"))
  }
  
  #### Landings 4
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 3 & values$objsp$sp[4] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[4]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 6
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[4],"(Kg/Ano)"))
  }
  
  #### Landings 5
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 4 & values$objsp$sp[5] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[5]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 7
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[5],"(Kg/Ano)"))
  }
  
  #### Landings 6
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 5 & values$objsp$sp[6] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[6]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 8
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[6],"(Kg/Ano)"))
  }
  
  #### Landings 7
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 6 & values$objsp$sp[7] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[7]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 9
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[7],"(Kg/Ano)"))
  }
  
  #### Landings 8
  if ("Arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 7 & values$objsp$sp[8] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "kg", species = values$objsp$sp[8]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 10
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Arribos", values$objsp$sp[8],"(Kg/Ano)"))
  }
  
  #### Income 1
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 0 & values$objsp$sp[1] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[1]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 11
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[1],"(Pesos/Ano)"))
  }
  
  #### Income 2
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 1 & values$objsp$sp[2] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[2]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 12
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[2],"(Pesos/Ano)"))
  }
  
  #### Income 3
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 2 & values$objsp$sp[3] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[3]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 13
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[3],"(Pesos/Ano)"))
  }
  
  #### Income 4
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 3 & values$objsp$sp[4] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[4]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 14
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[4],"(Pesos/Ano)"))
  }
  
  #### Income 5
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 4 & values$objsp$sp[5] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[5]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 15
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[5],"(Pesos/Ano)"))
  }
  
  #### Income 6
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 5 & values$objsp$sp[6] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[6]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 16
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[6],"(Pesos/Ano)"))
  }
  
  #### Income 7
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 6 & values$objsp$sp[7] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[7]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 17
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[7],"(Pesos/Ano)"))
  }
  
  #### Income 8
  if ("Ingresos por arribos de especies objetivo" %in% values$indS & length(values$objsp$sp) > 7 & values$objsp$sp[8] %in% available_sp) {
    model <- landings(data = data, location = values$comunidad, type = "price", species = values$objsp$sp[8]) %>%
      turfeffect(type = "soc", year.imp = values$ano.imp)
    
    TidyModel <- model$TidyModel %>%
      filter(term == "Post1")
    
    soc_data <- model$soc_data
    
    model <- model$model
    
    n_obj <- 18
    
    results$e[n_obj] <- TidyModel$estimate
    results$p[n_obj] <- TidyModel$p.value
    results$string[n_obj] <- valueBoxString(TidyModel)
    results$color[n_obj] <- soc_score(TidyModel)
    results$model[[n_obj]] <- model
    results$plot[[n_obj]] <- soc_plot(soc_data, y.lab = paste("Ingresos por arribos", values$objsp$sp[8],"(Pesos/Ano)"))
  }
  
  return(results)
}
