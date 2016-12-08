#' Select indicators
#'
#' @description Socioeconomical indicators selection function
#'
#' @param x An character vector containing the column numbers from the list
#'
#' @return
#'
#' @export
#'
#' @author Caio Faro
#'

indS_sel <- function(x) {

  library(dplyr)

  dataS <- read.csv(file = "./data/IndListS.csv", header = T)


  selected <- mutate(dataS, selected = rowSums(select(dataS, x))) %>%
    select(selected)

  IndListS <- dataS$Indicators[selected < 0]


  return(IndListS)

}

