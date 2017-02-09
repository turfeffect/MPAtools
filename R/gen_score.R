#' General Score
#'
#' @description According to the percentage of indicators that are positive, return a color based on a red - orange - yellow - green - olive scale, define by 20% intervals
#'
#' @param percent A number, between 0 and 100 that represents the percentage of positive indicators
#'
#' @return a color
#'
#' @export
#'


gen_score <- function(percent) {
  score = "olive"
  if (percent <= 80) {
    score = "green"
  }
  if (percent <= 60) {
    score = "yellow"
  }
  if (percent <= 40) {
    score = "orange"
  }
  if (percent <= 20) {
    score = "red"
  }
  return(score)
}
