bio_model <- function(data, covars){

  # using all covariates

  if (length(covars) == 3){
    model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Temperatura + Visibilidad + Profundidad, data = data)
  }

  # for only one variable

  if(length(covars) == 1 & !covars == "None"){

    # using only T

    if (covars == c("Temperatura")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Temperatura, data = data)
    }

    # using only V

    if (covars == c("Visibilidad")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Visibilidad, data = data)
    }

    # using only P

    if (covars == c("Profundidad")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Profundidad, data = data)
    }

  }

  if (length(covars) == 2){

    covars2 = paste(covars[1], covars[2])

    # using T and V

    if (covars2 == c("Temperatura Visibilidad")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Temperatura + Visibilidad, data = data)
    }

    # using T and P

    if (covar2s == c("Temperatura Profundidad")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Temperatura + Profundidad, data = data)
    }

    # using V and P

    if (covars2 == c("Visibilidad Profundidad")){
      model <- lm(formula = Indicador ~ Ano + Zona+ Post * Zona + Visibilidad + Profundidad, data = data)
    }

  }

  # dropping all covariates

  if (covars == "None"){
    model <- lm(formula = Indicador ~ Ano + Zona + Post * Zona, data = data)
  }

  return(model)

}
