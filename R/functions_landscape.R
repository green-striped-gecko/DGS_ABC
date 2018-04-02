# This file contains all the functions of the workflow that simulate
# our landscapes.

# funct
simulate_landscape_discrete <- function(ncol,
                                        nrow,
                                        type = "mosaictess",
                                        ...){
    
    if (type == "saura") {
        tempmask <- secr::make.mask(nx = ncol,
                                    ny = nrow,
                                    spacing = 1)
        r <- secr::raster(randomHabitat(tempmask,
                                        p = 0.5,
                                        A = 0.5))   
    }
    
    if (type == "randomcluster") {
        r <- nlm_randomcluster(ncol,
                               nrow,
                               p = ifelse(!exists("p"),
                                          0.5,
                                          p),
                               ai = if (!exists("ai")){
                                           c(0.5,0.5)} else ai)
    }
    
    if (type == "mosaictess") {
        r <- nlm_mosaictess(ncol, 
                            nrow,
                            germs = ifelse(!exists("germs"), 
                                           (ncol*nrow)/5,
                                           germs))
    }
    
    if (type == "fbm") {
        r <- nlm_fbm(ncol,
                nrow,
                fract_dim = ifelse(!exists("fract_dim"),
                                   0.8, 
                                   fract_dim))
    }
    
    if (type == "curds") {
        r <- nlm_curds(curds = ifelse(!exists("curds"), 
                                 c(0.5, 0.3, 0.6), 
                                 curds),
                  recursion_steps = if (!exists("recursion_steps")) {
                                           c(8, 4, 2)} else recursion_steps)
    }
    
    
    # binarize continouos landscapes
    if (length(raster::values(r)) > 2) {
        r <- util_classify(r, if (!exists("ratio")) {
                                 c(0.5, 0.5)} else ratio)
    }
    
    return(r)
}


# function to set landscape resistance
# x: landscape raster
# resistance: numeric resistance value
# prob: ratio between matrix & habitat, default 0.5

set_resistance <- function(landscape, resistance){

    #set non-habitat to friction values of resistance
    raster::values(landscape)[raster::values(landscape) > 0] <- resistance
    return(landscape)

}
