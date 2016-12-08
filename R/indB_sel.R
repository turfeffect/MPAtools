
#############################
# Biophysical indicators selection function
# Caio Faro
# 12/07/2016
#############################

indB_sel <- function(objective) {

  if (any(objective == "A")){
    bio_sel = c("Densidad",
              "Biomasa",
              "Organismos > LT_50")
  }

  else {
    bio_sel = c("Densidad",
                 "Biomasa")
  }

  return(bio_sel)

}

