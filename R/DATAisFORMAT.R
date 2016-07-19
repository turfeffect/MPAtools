#' AisA
#'
#' @description
#'
#' @param data a data.frame that will be compared to a data.frame with the specified format as specified by the COBIApp
#'
#' @param format The specified format to which data will be compared
#'
#' @return message an error message displayed if the data is not consistent with format
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>


DATAisFORMAT <- function (data, format){

  message=c("Formato correcto")

  if (format == "A"){

    if (!length(data)==33){
      message <- c("El archivo cargado NO coincide con el formato especificado.")
    }
  } else if (format == "B"){

    if(!length(data) == 28 | !any(data$Abundancia > 1)){
      message <- c("El archivo cargado NO coincide con el formato especificado")
    }

  } else if (format == "C") {

    if (!length(data) == 28 | any(data$Abundancia > 1)){
      message <- c("El archivo cargado NO coincide con el formato especificado")
    }
  }

  return(message)
}
