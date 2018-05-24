# This file contains all the functions of the workflow that simulate
# our landscapes.


nlm_randomcluster <- function(ncol, nrow,
															
															resolution = 1,
															
															p,
															
															ai = c(0.5, 0.5),
															
															neighbourhood = 4,
															
															rescale = TRUE) {
	
	
	
	# Check function arguments ----
	
	checkmate::assert_count(ncol, positive = TRUE)
	
	checkmate::assert_count(nrow, positive = TRUE)
	
	checkmate::assert_numeric(resolution)
	
	checkmate::assert_numeric(p)
	
	checkmate::assert_true(p <= 1)
	
	checkmate::assert_numeric(ai)
	
	checkmate::assert_true(neighbourhood == 4 || neighbourhood == 8)
	
	checkmate::assert_logical(rescale)
	
	
	
	# Step A - Create percolation map
	
	
	
	nlm_percolation <- function(ncol, nrow, prob, resolution){
		
		
		
		percolation_matrix <- matrix(NA, nrow = nrow, ncol = ncol)
		
		
		
		percolation_matrix[] <- vapply(
			
			percolation_matrix,
			
			function(x) {
				
				ifelse(stats::runif(1, 0, 1) < prob, TRUE, FALSE)
				
			},
			
			logical(1)
			
		)
		
		
		
		percolation_raster <-
			
			raster::raster(percolation_matrix)
		
		
		
		# specify resolution ----
		
		raster::extent(percolation_raster) <- c(
			
			0,
			
			ncol(percolation_raster) * resolution,
			
			0,
			
			nrow(percolation_raster) * resolution
			
		)
		
		
		
		percolation_raster
		
	}
	
	
	
	ranclumap <- nlm_percolation(ncol, nrow, p, resolution = resolution)
	
	
	
	
	
	
	
	
	
	# Step B - Cluster identification (clustering of adjoining pixels)
	
	ranclumap <- raster::clump(ranclumap, direction = neighbourhood, gaps = FALSE)
	
	
	
	# Step C - Cluster type assignation
	
	# number of different cluster
	
	numclu <- max(raster::values(ranclumap), na.rm = TRUE)
	
	# assign to each cluster nr a new category given by Ai
	
	clutyp <- sample(seq_along(ai), numclu, replace = TRUE, prob = ai)
	
	# write back new category nr
	
	raster::values(ranclumap) <- clutyp[raster::values(ranclumap)]
	
	
	
	# Step D - Filling the map
	
	# helperfuction to choose values
	
	fillit <- function(cid) {
		
		# get neighbour cells
		
		nbrs <- raster::adjacent(ranclumap, cid, directions = 8, pairs = FALSE)
		
		# count neighbour values (exclude NA see Saura 2000 paper)
		
		vals <- table(raster::values(ranclumap)[nbrs])
		
		# if everything in da hood is NA
		
		if (purrr::is_empty(vals)) {
			
			# be a rebel get your own value
			
			fili <- sample(seq_along(ai), 1, prob = ai)
			
		}else{
			
			# if there is a majority be an prick and join the winning team
			
			fili <- as.integer(names(vals)[vals == max(vals)])
			
			if (length(fili) > 1) {
				
				# if there is a tie just join a faction
				
				fili <- sample(fili, 1)
				
			}
			
		}
		
		# choose your destiny
		
		return(fili)
		
	}
	
	
	
	# identify unfilled cells
	
	gaps <- dplyr::rowwise(tibble::tibble(
		
		ctf = (1:(ncol * nrow))[is.na(raster::values(ranclumap))]
		
	))
	
	# get values for the gaps
	
	gaps <- dplyr::mutate(gaps, val = fillit(ctf))
	
	# feed it back in the map
	
	raster::values(ranclumap)[gaps$ctf] <- gaps$val
	
	
	
	# specify resolution ----
	
	raster::extent(ranclumap) <- c(
		
		0,
		
		ncol(ranclumap) * resolution,
		
		0,
		
		nrow(ranclumap) * resolution
		
	)
	
	
	
	return(ranclumap)
	
}







# funct
simulate_landscape_discrete <- function(ncol,
                                        nrow,
                                        type = "mosaictess",
                                        ...){
    
    if (type == "saura") {
    	  tempmask <- secr::make.mask(nx = ncol,
    	  														ny = nrow,spacing=1
    	  														)
    
        r <- secr::raster(randomHabitat(tempmask,
                                        p = 0.5,
                                        A = 0.5))   
        r[is.na(r)] <- 0
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
  #  if (length(raster::values(r)) > 2) {
  #      r <- util_classify(r, if (!exists("ratio")) {
  #                               c(0.5, 0.5)} else ratio)
  #  }
    
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
