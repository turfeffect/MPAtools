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

  data(IndListS)

  dataS <- IndListS


  selected <- mutate(dataS, selected = rowSums(select(dataS, x))) %>%
    select(selected)

  IndListS <- dataS$Indicators[selected < 0]


  return(IndListS)

}

