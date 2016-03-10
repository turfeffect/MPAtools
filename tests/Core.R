
library(devtools)
load_all()

area=matrix(0.4,nrow=50, ncol=50)

# Area 1, a single large MPA

area1=area
area1[16:32,16:32]=0

# Area 2, a single large MPA with fishing the line effect

area2=area
area2[23:27,23:27]=0
for (i in 1:23){
  area2[,23-i]=0.4/i
  area2[,27+i]=0.4/i
  area2[23-i,]=0.4/i
  area2[27+i,]=0.4/i
}

# Area 3, a singe large MPA with a smaller permiter / area ratio AND fishing the line effect

area3=area
area3[13:37,25]=0
for (i in 1:24){
  area3[,24-i]=0.4/i
  area3[,26+i]=0.4/i
}

area3[1:11,21:29]=t(area4[21:29,12:22])
area3[39:50,21:29]=t(area4[21:29,28:39])


# Area 4, Several small MPAs

area4=area

area4[10:12,10:11]=0
area4[38:40,39:40]=0
area4[10:12,39:40]=0
area4[38:40,10:11]=0
area4[25,25]=0

# Area 5, a Several small MPA with fishing the line effect


# Area 6, a Several small MPA with a smaller permiter / area ratio AND fishing the line effect


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



