#############################
# Socioeconomic indicators selection function
# Caio Faro
# 12/07/2016
#############################

indS_sel <- function(x) {

  library(dplyr)

  dataS <- read.csv(file = "./data/IndListS.csv", header = T)


  IndListS <- mutate(dataS, selected = rowSums(select(dataS, x))>0) %>%
    select(selected)


  return(IndListS)

}

