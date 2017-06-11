#' Title
#'
#' @param data
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
define_covars <- function(data, cutoff = 0.1){

  data_length <- dim(data)[1]

  naT <- sum(is.na(data$Temperatura))/data_length
  naV <- sum(is.na(data$Visibilidad))/data_length
  naP <- sum(is.na(data$Profundidad))/data_length

  naCovars <- data.frame(Temperatura = naT, Visibilidad = naV, Profundidad = naP) %>%
    gather(covar, value) %>%
    filter(value <= cutoff)

  if(dim(naCovars)[1] >= 1){
    return(naCovars$covar)
  }

  if(dim(naCovars)[1] == 0){
    return("None")
  }

}
