#' Select indicators
#'
#' @description Biophysical indicators selection function
#'
#' @param x An character vector containing the column numbers from the list
#'
#' @return
#'
#' @export
#'
#' @author Caio Faro
#'

indB_sel <- function(x) {

  library(dplyr)

  data(IndListB)

  dataB <- IndListB


  selected <- mutate(dataB, selected = rowSums(select(dataB, x))) %>%
    select(selected)

  IndListB <- dataB$Indicators[selected>0]

  return(IndListB)

}
