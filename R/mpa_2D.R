#======PART 2====================================================
#Creates a simple MPA model with logistic growth
#r=rate of increase, K=carrying capacity, u.out=harvest rate outside MPA,
#ncells=number of cells for model, nsteps=number of time periods,
#MPA.width=number of cells in MPA, mrate=movement rate
#===================================================================

mpa_2D=function(r, pop0, K, area, nsteps, mrate) {

   # First we call calc.harvest, that returns a vectos whic is our simulated MPA. 0=mpa, number=rate of harvest.

   # u.vec <- calc.harvest.vec(u.out=u.out, ncells=ncells, MPA.width=MPA.width)

  # This will be removed and put into another function that stands alone
  # area is a matrix of 3 by 3. Every cell has a value of 0.1 (harvest rate) except for the one in the middle (area[2,2]) which is 0 (i.e. that cell is the MPA).
u.vec=area

  size=dim(u.vec)

  nrows=size[1]
  ncols=size[2]

   pop <- matrix(nrow=nrows, ncol=ncols)

   #set starting population in vector pop in each cell equal to K

   pop[]=pop0

   #vector left.cells storing the position of cells to the left

   left.cells=c(ncols, 1:(ncols-1))

   #vector right.cells storing the position of cells to the right

   right.cells=c(2:ncols,1)

   #vector up.cells storing the positon of cells up

   up.cells=c(nrows,1:(nrows-1))

   #vector down.cells storing the position of cells down

   down.cells=c(2:nrows, 1)

   # These keep track of total population size in MPAs (pop.in) and
   # total population size in fishing grounds (pop.out).
   # ones is a matrix of size pop, that is used to normalize population by
   # number of cells (i.e. population size/number of cells) later in the
   # final plot of pop vs time.

   inside=u.vec==0
   outside=u.vec>0
   ones=matrix(1,nrow=nrows, ncol=ncols)
   pop.in=sum(pop[inside], na.rm=TRUE)/sum(ones[inside], na.rm=TRUE)
   pop.out=sum(pop[outside], na.rm=TRUE)/sum(ones[outside], na.rm=TRUE)
   total.catches=sum(u.vec*pop, na.rm=TRUE)

   #This plots the "gaussian shaped" thing, across time, for a vector mpa. I will coment it to change a plot for contour, so we can see our 2D MPA.
   #plot the initial numbers
   #plot(x=1:ncells, y=pop, xlab="Cell number", lwd=3, ylab="Population size", ylim=c(0, 1.05*max(pop)), type="l", yaxs="i", xaxs="i")

   filled.contour(pop, color=terrain.colors, asp=1, main="Initial time")

   #loop through the time steps
   for (i in 1:nsteps) {
      #Vector "leaving" of number leaving each cell
      #is population size times diffusion rate

     leaving=pop*mrate

      #The number of immigrants is 1/4 those leaving cells to the
      #left, 1/4 those leaving cells to the right, 1/4 those leaving up
      #and 1/4 those leaving down. This is a complicated expression
      #take a minute to think it through!

     arriving=0.25*leaving[left.cells]+
       0.25*leaving[right.cells]+
       0.25*leaving[up.cells]+
       0.25*leaving[down.cells]

      #surplus production from the logistic model

     surplus=(r*pop)*(1-(pop/K))


      #catches = harvest rate in each cell times the population size

      catches=u.vec*pop

      #update the population numbers

      pop=pop+surplus-catches-leaving+arriving

      #Also save two vector containing population size inside MPA (pop.in)
      #and population size outside MPA (pop.out)
      pop.in[i+1]=sum(pop[inside], na.rm=TRUE)/sum(ones[inside], na.rm=TRUE)
      pop.out[i+1]=sum(pop[outside], na.rm=TRUE)/sum(ones[outside], na.rm=TRUE)
      total.catches[i+1]=sum(catches, na.rm=TRUE)


      #plot the population in each cell. It is comented because that is for 1D
      #lines(x=1:ncells, y=pop, lwd=(nsteps-i+1)/nsteps*3)

      #Plot population in each cell, for a 2D MPA
      filled.contour(pop, col=rainbow(100), asp=1, main=i)
   }

   results=list(pop=pop, time.series=data.frame(time=seq(0:nsteps), pop.in, pop.out, total.catches))

   return(results)
}
