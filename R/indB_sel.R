
#############################
# Biophysical indicators selection function
# Caio Faro
# 12/07/2016
#############################

indB_sel <- function(x) {

  library(dplyr)

  dataB <- read.csv(file = "./data/IndListB.csv", header = T)


  IndListB <- mutate(dataB, selected = rowSums(select(dataB, x))>0) %>%
    select(selected)


  return(IndListB)

}
