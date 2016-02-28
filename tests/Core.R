
area=matrix(nrow=30, ncol=30)
area[]=0.1
area[5:9,5:9]=0
area[20:24, 20:24]=0


windows()
JC=MPA.model(r=0.2, pop0=500, K=1000, area=area, nsteps=10, mrate=0.4)

windows()
plot(JC$time.series$time, JC$time.series$pop.in, col="blue", type="b" ,xlab="Time (steps)", ylab="Population density (N/cell)")
lines(JC$time.series$time, JC$time.serie$pop.out, col="red", type="b")

