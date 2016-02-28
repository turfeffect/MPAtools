
area=matrix(0.1,nrow=30, ncol=30)
area[5:9,5:9]=0
area[20:24, 20:24]=0

area=matrix(0.1, nrow=300, ncol=300)
area[10:19,10:19]=0

for (i in 1:10){
  area[,10-i]=0.1/i
  area[,20+i]=0.1/i
  area[10-i,]=0.1/i
  area[20+i,]=0.1/i
}

JC=mpa_2D(r=0.2, pop0=750, K=1000, area=area, nsteps=50, mrate=0.4, op=FALSE)

windows()
plot(JC$time.series$time, JC$time.series$pop.in, col="blue", type="b" ,xlab="Time (steps)", ylab="Population density (N/cell)", ylim=c(min(JC$time.series$pop.out),(1000*1.1)))
lines(JC$time.series$time, JC$time.series$pop.out, col="red", type="b")
abline(a=1000, b=0, lty=2)
abline(a=500, b=0, lty=2)

windows()
plot(JC$time.series$time, JC$time.series$total.catches, col="black", type="b") #this on a second axis

windows()
plot(JC$time.series$time, JC$time.series$pop, col="black", type="b")



