#' Perform Difference-in-Difference analysis
#'
#' @description Performs a difference-in-difference analysis on a series of data and compares the effect of the reserve.
#'
#' @param data A dataframe that has been created with any of the following functions: richness(), density(), fish_biomass(), fish_size(), trophic(). All these functions are part of this package.
#' @param year.imp An integer that indicates the year when the reserve was first implemented.
#'
#' @return Returns a linear model, where the value of the interactive term between Zone and Year is the difference in difference value.

did=function(data, year.imp){

  library(dplyr)
  library(tidyr)

  columns=c(colnames(data)[-ncol(data)], "Interest")
  colnames(data)=columns

  data=data %>%
    filter(Year==min(Year)|
             Year==max(Year))

  data$Year2[data$Year<=year.imp]=0
  data$Year2[data$Year>year.imp]=1
  data$Zone2[data$Zone=="Reserve"]=1
  data$Zone2[data$Zone=="Fished"]=0

  model=lm(Interest~Year2 + Zone2 +Year2*Zone2, data=data)

  return(model)
}
