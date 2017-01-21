#' Scoring system
#'
#' @param x
#'
#' @return score
#' @export
#'

score <- function(x){
  est = x$est
  p = x$p

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
