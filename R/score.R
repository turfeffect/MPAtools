#' Scoring system
#'
#' @param x
#'
#' @return score
#' @export
#'

score <- function(model){

  model <- summary(model)

  est = coefficients(model)[7]
  p = coefficients(model)[28]

  if (est > 0 & p < 0.05) {
    score = "olive"
  }

  if (est > 0 & p > 0.05){
    score = "green"
  }

  if (est == 0){
    score = "yellow"
  }

  if (est < 0 & p > 0.05){
    score = "orange"
  }

  if (est < 0 & p < 0.05){
    score = "red"
  }

  return(score)

}
