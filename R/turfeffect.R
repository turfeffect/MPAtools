#' turfeffect
#'
#' @param data A data.frame generated by any of the following functions: richness(), density(), trophic(), fish_biomass(), and fish_size().
#' @param reserve A string
#' @param control A string
#' @param type A string
#'
#' @return An object of class "lm"
#'
#' @export

turfeffect <- function (data, reserve = NULL, control = NULL, type = NULL, year.imp = NULL){

  library(tidyverse)

  soc_data <- NULL

  data <- data %>%
    mutate(Post = as.factor(ifelse(Ano <= year.imp, 0, 1)),
           Ano = as.factor(Ano))

  if (type == "bio"){

    model <- filter(data, Sitio %in% reserve | Sitio %in% control) %>%
      bio_model(covars = covars_wrapper(., year.imp = year.imp))

    TidyModel <- model%>%
      lmtest::coeftest(vcov = sandwich::vcovHC(., type = "HC1")) %>%
      broom::tidy()

  } else if (type == "soc"){

    model <- lm(Indicador ~ Post, data = data)

    TidyModel <- model %>%
      lmtest::coeftest(vcov = sandwich::vcovHC(., type = "HC1")) %>%
      broom::tidy()

    soc_data <- data
  }

  return(list(TidyModel = TidyModel, model = model, soc_data = soc_data))
}
