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
    score = "green2.jpg"
  }

  if (est > 0 & p > 0.05){
    score = "green1.jpg"
  }

  if (est == 0){
    score = "yellow.jpg"
  }

  if (est < 0 & p > 0.05){
    score = "orange.jpg"
  }

  if (est < 0 & p < 0.05){
    score = "red.jpg"
  }

  return(score)

}
