#############################
# Socioeconomic indicators selection function
# Caio Faro
# 12/07/2016
#############################

indS_sel <- function(objective) {

  if (any(objective == "A")){
    sec_sel = c("Arribos")
  }

  else {
    sec_sel = c("Arribos",
                "Ingresos por arribos")
  }

  return(sec_sel)

}
