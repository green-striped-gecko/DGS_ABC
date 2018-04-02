simulate_landscape_discrete <- function(ncol,
                               nrow,
                               type,
                               resistance = 5, 
                               ...){
    
    if (type = saura){
        tempmask <- secr::make.mask(nx=nx,ny=ny,spacing=1)
        r <- secr::raster(randomHabitat(tempmask, p = 0.5, A = 0.5))   
    }
    
    if (){
        
    }
    
    #set non-habitat to friction values of resistance
    values(r)[is.na(values(r))==T]<- 10
}

#create a landscape
nx=50
ny=50

#for continuous landscapes etc marcos generator
set.seed(555) #(to make sure we have the same example running)
tempmask<-   secr::make.mask(nx=nx,ny=ny,spacing=1)
r <-         raster::raster(secr::randomHabitat(tempmask, p = 0.5, A = 0.5))
#set non-habitat to friction values of 10
values(r)[is.na(values(r))==T]<- 10
plot(r)
landscape<- r 