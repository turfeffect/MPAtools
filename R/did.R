#' Perform Difference-in-Difference analysis
#'
#' @description Performs a difference-in-difference analysis on a series of data and compares the effect of the reserve. It is based on a linear model, where the variable of interest is regressed on dummy variables composed by Ano and Zonificacion, and an interaction term between both. The model is OutcomeVariable = b0 + b1*Ano + b2*Zonificacion + b3(Ano*Zonificacion). Here, Ano indicates pre and post implementation of the reserve. Zonificacion serves as a a way to compare a trend and allows to compare between the reserve and control sites. The DD value is indicated by the coefficient of the interaction term b3.
#'
#' @param data A dataframe that has been created with any of the following functions: richness(), density(), fish_biomass(), fish_size(), trophic(). All these functions are part of this package.
#' @param year.imp An integer that indicates the year when the reserve was first implemented.
#' @param year.int An integer that indicates the year of interest to perform the DiD analysis
#'
#' @return Returns an objet of class "lm", where the value of the interactive term between Zonificacion and Ano (Zonificacion and Ano) is the difference-in-difference estimate.
#'
#' @export

did <- function(data, year.imp, year.int=NULL){
  library(dplyr) #Load dplyr
  library(tidyr) #Load tidyr

  if (is.null(year.int)){year.int=max(data$Ano)}

  colnames(data) <- c("Ano", "Zonificacion", "Sitio", "Transecto", "Indicador") #Reasign the new columnames to the data

  data <- data %>%              #Set data equal to data
    filter(Ano2 == year.imp|   #Filter by year to keep only year before treatment
             Ano2 == year.int) #and latest year or year indicated by user in year.int

  Ano <-  seq(1:length(data$Ano))
  Zonificacion <- seq(1:length(data$Ano))

  # Create dummy variables
  Ano[data$Ano2 <= year.imp] <- 0   #Anos prior to implementation are set to 0
  Ano[data$Ano2 > year.imp] <- 1    #Anos after implementation are set to 1
  Zonificacion[data$Zonificacion == "Pesca"] <- 1  #Reserve site (i.e. under treatment) is set to 1
  Zonificacion[data$Zonificacion == "No Pesca"] <- 0   #Fished site (i.e. without treatment) is set to 0

  dummy.data <- data.frame(Ano = Ano,
                           Zonificacion = Zonificacion,
                           Indicador = data$Indicador)

  model=lm(Indicador ~ Ano*Zonificacion, data = dummy.data) #Calculate the linear model on the data

  return(model) #Return the model
}
