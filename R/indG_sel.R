#' Select indicators
#'
#' @description Governance indicators selection function
#'
#' @param x An character vector containing the column numbers from the list
#'
#' @return
#'
#' @export
#'
#' @author Caio Faro
#'

indG_sel <- function(x) {

  library(dplyr)

  data(IndListG)

  dataG <- IndListG


  selected <- mutate(dataG, selected = rowSums(select(dataG, x))) %>%
    select(selected)

  IndListG <- dataG$Indicators[selected > 0]

  return(IndListG)

}

