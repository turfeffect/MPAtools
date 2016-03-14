mpa_xls2Rdata=function(a=1){
library(readxl)

area1=as.matrix(read_excel("data/areas.xlsx", sheet=1, col_names=FALSE))
area2=as.matrix(read_excel("data/areas.xlsx", sheet=2, col_names=FALSE))
area3=as.matrix(read_excel("data/areas.xlsx", sheet=3, col_names=FALSE))
area4=as.matrix(read_excel("data/areas.xlsx", sheet=4, col_names=FALSE))

save(area1, file="./data/area1.RData")
save(area2, file="./data/area2.RData")
save(area3, file="./data/area3.RData")
save(area4, file="./data/area4.RData")
}






