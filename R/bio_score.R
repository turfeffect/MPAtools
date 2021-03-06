#' Scoring system
#'
#' @param model a linear model generated by the turfeffect function, where type has been set to "bio"
#'
#' @return score
#' @export
#'

bio_score <- function(model) {

  est = model$estimate
  p = model$p

  score = "yellow"

  if (est > 0 & p < 0.05) {
    score = "olive"
  }

  if (est > 0 & p >= 0.05 & p < 0.1) {
    score = "green"
  }

  if (est < 0 & p >= 0.05 & p < 0.1) {
    score = "orange"
  }

  if (est < 0 & p < 0.05) {
    score = "red"
  }

  return(score)

}
