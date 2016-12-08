#############################
# Governance indicators selection function
# Caio Faro
# 12/07/2016
#############################

indG_sel <- function(x) {

  library(dplyr)

  dataG <- read.csv(file = "./data/IndListG.csv", header = T)


  IndListG <- mutate(dataG, selected = rowSums(select(dataG, x))>0) %>%
    select(selected)


  return(IndListG)

}

