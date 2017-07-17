#' A wrapper for define_covars
#'
#' @description Identifies for each sampling point (before-after-control-reserve) which covariates to use using define_covars. It then identifies the common covariates available for each sampling point and returns a string that bio_model can use to implement the correct model
#'
#' @param data dataset
#' @param year.imp year of implementation
#'
#' @return a vector stating which covariates to be used
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%

covars_wrapper <- function(data, year.imp){

  data$Ano = as.numeric(as.character(data$Ano))

  before_reserve <- data %>%
    filter(Ano <= year.imp, Zona == "Reserva") %>%
    define_covars()

  after_reserve <- data %>%
    filter(Ano > year.imp, Zona == "Reserva") %>%
    define_covars()

  before_control <- data %>%
    filter(Ano <= year.imp, !Zona == "Reserva") %>%
    define_covars()

  after_control <- data %>%
    filter(Ano > year.imp, !Zona == "Reserva") %>%
    define_covars()

  covars <- c("Temperatura", "Visibilidad", "Profundidad")

  length_covars <- data.frame(set = c("BR", "AR", "BC", "AC"),
                           Temperatura = NA,
                           Vibilididad = NA,
                           Profundidad = NA)

  if("Temperatura" %in% before_reserve){
    length_covars$Temperatura[1] <- 1
  }

  if("Temperatura" %in% after_reserve){
    length_covars$Temperatura[2] <- 1
  }

  if("Temperatura" %in% before_control){
    length_covars$Temperatura[3] <- 1
  }

  if("Temperatura" %in% after_control){
    length_covars$Temperatura[4] <- 1
  }

  #
  if("Profundidad" %in% before_reserve){
    length_covars$Profundidad[1] <- 1
  }

  if("Profundidad" %in% after_reserve){
    length_covars$Profundidad[2] <- 1
  }

  if("Profundidad" %in% before_control){
    length_covars$Profundidad[3] <- 1
  }

  if("Profundidad" %in% after_control){
    length_covars$Profundidad[4] <- 1
  }

  #
  if("Visibilidad" %in% before_reserve){
    length_covars$Visibilidad[1] <- 1
  }

  if("Visibilidad" %in% after_reserve){
    length_covars$Visibilidad[2] <- 1
  }

  if("Visibilidad" %in% before_control){
    length_covars$Visibilidad[3] <- 1
  }

  if("Visibilidad" %in% after_control){
    length_covars$Visibilidad[4] <- 1
  }

  covars <- length_covars %>%
    gather(var, value, -set) %>%
    group_by(var) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    filter(value == max(.$value), value > 0) %$%
    unique(var)

  if(length(covars)<1){
    return("None")
  }

  if(length(covars)>=1){
    return(covars)
  }

}
