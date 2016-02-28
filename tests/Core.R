
area=matrix(nrow=30, ncol=30)
area[]=0.1
area[5:9,5:9]=0
area[20:24, 20:24]=0


windows()
JC=mpa_2D(r=0.2, pop0=750, K=1000, area=area, nsteps=50, mrate=0.1)

windows()
plot(JC$time.series$time, JC$time.series$pop.in, col="blue", type="b" ,xlab="Time (steps)", ylab="Population density (N/cell)", ylim=c(min(JC$time.series$pop.out),(1000*1.1)))
lines(JC$time.series$time, JC$time.series$pop.out, col="red", type="b")
plot(JC$time.series$time, JC$time.series$total.catches, col="black", type="b") #this on a second axis

abline(a=1000, b=0, lty=2)
abline(a=500, b=0, lty=2)


