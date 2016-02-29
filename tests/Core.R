
library(devtools)
load_all()

area=matrix(0.4,nrow=50, ncol=50)

# Area 1, a single large MPA

area1=area
area1[23:27,23:27]=0

# Area 2, a single large MPA with a smaller perimeter / area ratio

area2=area
area2[13:37,25]=0

# Area 3, a single large MPA with fishing the line effect

area3=area
area3[23:27,23:27]=0
for (i in 1:23){
  area3[,23-i]=0.4/i
  area3[,27+i]=0.4/i
  area3[23-i,]=0.4/i
  area3[27+i,]=0.4/i
}

# Area 4, a singe large MPA with a smaller permiter / area ratio AND fishing the line effect

area4=area
area4[13:37,25]=0
for (i in 1:24){
  area4[,24-i]=0.4/i
  area4[,26+i]=0.4/i
}

area4[1:11,21:29]=t(area4[21:29,12:22])
area4[39:50,21:29]=t(area4[21:29,28:39])


# Area 5, Several small MPA

area5=area

area5[10:12,10:11]=0
area5[38:40,39:40]=0
area5[10:12,39:40]=0
area5[38:40,10:11]=0
area5[25,25]=0

# Area 6, a Several small MPA with a smaller perimeter / area ratio

area6=area
area6[10:12,10:11]=0
area6[14:16,10:11]=0
area6[18:20,10:11]=0
area6[22:24,10:11]=0

# Area 7, a Several small MPA with fishing the line effect


# Area 8, a Several small MPA with a smaller permiter / area ratio AND fishing the line effect


# All scenarios
windows()
par(mfrow=c(2,4))
image(area1)
image(area2)
image(area3)
image(area4)
image(area5)
image(area6)




JC=mpa_sim(r=0.2, pop0=500, K=1000, area=area1, nsteps=50, mrate=0.1, op=TRUE)

windows()
plot(JC$time.series$time, JC$time.series$pop.in, col="blue", type="b" ,xlab="Time (steps)", ylab="Population density (N/cell)", ylim=c(min(JC$time.series$pop.out),(1000*1.1)))
lines(JC$time.series$time, JC$time.series$pop.out, col="red", type="b")
abline(a=1000, b=0, lty=2)
abline(a=500, b=0, lty=2)

windows()
plot(JC$time.series$time, JC$time.series$total.catches, col="black", type="b") #this on a second axis

windows()
plot(JC$time.series$time, JC$time.series$pop, col="black", type="b")



