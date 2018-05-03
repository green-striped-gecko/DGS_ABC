library(PopGenReport)
library(secr)     
#create a landscape
nx=50
ny=50


#for continuous landscapes etc marcos generator
set.seed(333) #(to make sure we have the same example running)
tempmask<-secr::make.mask(nx=nx,ny=ny,spacing=1)
r <- secr::raster(randomHabitat(tempmask, p = 0.5, A = 0.5))
#set non-habitat to friction values of 10
values(r)[is.na(values(r))==T]<- 10
plot(r)
landscape<- r 


#make sure n populations are mindist apart and in habitat
############################
createpops <- function(n, mindist, landscape, plot=TRUE)
{  
  minx <- raster::extent(landscape)@xmin #get the min and max  coordinates
  miny <- raster::extent(landscape)@ymin #coordinates of the landscape
  maxx <- raster::extent(landscape)@xmax
  maxy <- raster::extent(landscape)@ymax
  
  cc<- 1
  coords <- data.frame(lx=NA, ly=NA)
  while (cc<= n )  #repeat until you have found n locations
  {
    draw=FALSE
    while (draw==FALSE)
    {
      x <- runif(1,minx,maxx)
      y <- runif(1,miny,maxy)
      if (landscape[cellFromXY(landscape,c(x,y) )]==1)  draw=TRUE 
      #check if in the habitat
    }
    
    coords[cc,] <- c(x,y)
    
    if (nrow(coords)>1) d <- min(dist(coords)) else d <- mindist+1 
    
    if (d > mindist) cc <- cc+1  
    #take the location only if distance is larger than mindist
  }
  if (plot==TRUE) 
  {
    plot(landscape)  
    points(coords, pch=16)
  }
  return( as.matrix( coords))
}



##### define all parameters for a single run.....

para<- list()
#Define populations (dynamics)
para$n.pops=8
para$n.ind=50

para$sex.ratio <- 0.5
#age distribution....

para$n.cov <- 3 
#number of covariates (before the loci in the data.frame, do not change this!!)
para$n.offspring = 2

#migration
para$mig.rate <- 0.1

#dispersal: exponential dispersal with maximal distance in map units
para$disp.max=50   #average  dispersal of an individual in meters
para$disp.rate = 0.05 #proportion of dispersing individuals

#Define genetics
para$n.allels <- 2
para$n.loci <- 50
para$mut.rate <- 0.00

para$method <- "leastcost" #rSPDdistance, commute (explore )
para$NN <- 8  #number of neighbours for the cost distance method


#using generated landscape in popgenreport

para$locs <-createpops(n=para$n.pops, mindist = 3, 
                       landscape = r, plot = TRUE)
#give the population some names 
rownames(para$locs) <- LETTERS[1:para$n.pops]


# Create a costdistance matrix 
cost.mat <- PopGenReport::costdistances(landscape, para$locs, para$method, para$NN) 
#needed for the simulation
eucl.mat <- as.matrix(dist(para$locs))  #needed for the analysis later

# Plot your landscape with the populations....

plot(landscape)
points(para$locs[,1], para$locs[,2], pch=16, cex=2, col="orange")
text(para$locs[,1],para$locs[,2], row.names(para$locs), cex=1.5)


simsteps <- 100

#reference data set
simpops <- PopGenReport::init.popgensim(para$n.pops, para$n.ind, para$sex.ratio,para$n.loci, para$n.allels, para$locs, para$n.cov )
gi <- PopGenReport::pops2genind(simpops)

#run for 100 generations
simpops <- PopGenReport::run.popgensim(simpops, steps=simsteps, cost.mat, n.offspring=para$n.offspring, n.ind=para$n.ind, para$mig.rate, para$disp.max, para$disp.rate,  para$n.allels, para$mut.rate, n.cov=para$n.cov, rec="none")
#convert to genind object (smaller)
gi <- PopGenReport::pops2genind(simpops)
sss.ref <- as.dist(pairwise.fstb(gi))





res <- data.frame(resistance=NA, sss=NA) 

tt <- round(proc.time()[3])

for (i in 1:1000)
{
  
#create a random resistance value between 1:30
  
resis <- runif(1,1,30)
landscape <- r
values(landscape)[values(r)==10] <- resis

  # Create a new costdistance matrix 
cost.mat <- PopGenReport::costdistances(landscape, para$locs, para$method, para$NN) 

#initiate pops
simpops <- PopGenReport::init.popgensim(para$n.pops, para$n.ind, para$sex.ratio,para$n.loci, para$n.allels, para$locs, para$n.cov )
  gi <- PopGenReport::pops2genind(simpops)

#run for 100 generations
simpops <- PopGenReport::run.popgensim(simpops, steps=simsteps, cost.mat, n.offspring=para$n.offspring, n.ind=para$n.ind, para$mig.rate, para$disp.max, para$disp.rate,  para$n.allels, para$mut.rate, n.cov=para$n.cov, rec="none")
#convert to genind object (smaller)
gi <- PopGenReport::pops2genind(simpops)
sss.dummy <- as.dist(pairwise.fstb(gi))
res[i, ]<- c(resis, sum((sss.dummy-sss.ref)^2))

cat(paste("Run",i,"."))
cat(paste("Took:", round((proc.time()[3]-tt) / 60,2),"minutes.\n"))
flush.console()
if (i %%20 == 0) {
  th <- quantile(res$sss,probs=0.2)
  plot(density(res$resistance[res$sss<th]))
  abline(v=10, col="red")
}

}


#now we need to write a loop around the single run changing the resistance value (proposal function) and store the summary statistics and the resistance value in a data.frame....

#generate the landscape with a different restistance (1:30)
#update the landscape
#calculate the new cost matrix
#run the simulation as before
#record summary statistc and restistance value


#github names
# marcosci
# sangeetabhatia03
# mvanack









