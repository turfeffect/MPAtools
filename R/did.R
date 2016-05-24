#' Perform Difference-in-Difference analysis
#'
#' @description Performs a difference-in-difference analysis on a series of data and compares the effect of the reserve. It is based on a linear model, where the variable of interest is regressed on dummy variables composed by Year and Zone, and an interaction term between both. The model is OutcomeVariable = b0 + b1*Year + b2*Zone + b3(Year*Zone). Here, Year indicates pre and post implementation of the reserve. Zone serves as a a way to compare a trend and allows to compare between the reserve and control sites. The DD value is indicated by the coefficient of the interaction term b3.
#'
#' @param data A dataframe that has been created with any of the following functions: richness(), density(), fish_biomass(), fish_size(), trophic(). All these functions are part of this package.
#' @param year.imp An integer that indicates the year when the reserve was first implemented.
#' @param year.int An integer that indicates the year of interest to perform the DiD analysis
#'
#' @return Returns a linear model, where the value of the interactive term between Zone and Year is the difference in difference value.

did=function(data, year.imp, year.int=NULL){
  library(dplyr) #Load dplyr
  library(tidyr) #Load tidyr

  if (is.null(year.int)){year.int=max(data$Year)}

  columns=c(colnames(data)[-ncol(data)], "Interest") #Reset colnames so that the variable of interest (richness, density, size, biomass, trophic level) is renamed
  colnames(data)=columns                             #Reasign the new columnames to the data

  data=data %>%              #Set data equal to data
    filter(Year==year.imp|   #Filter by year to keep only year before treatment
             Year==year.int) #and latest year or year indicated by user in year.int

  # Create dummy variables
  data$Year2[data$Year<=year.imp]=0   #Years prior to implementation are set to 0
  data$Year2[data$Year>year.imp]=1    #Years after implementation are set to 1
  data$Zone2[data$Zone=="Reserve"]=1  #Reserve site (i.e. under treatment) is set to 1
  data$Zone2[data$Zone=="Fished"]=0   #Fished site (i.e. without treatment) is set to 0

  model=lm(Interest~Year2 + Zone2 +Year2*Zone2, data=data) #Calculate the linear model on the data

  return(summary(model)) #Return the model
}
