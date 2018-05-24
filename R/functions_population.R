# n: number of populations
# mindist apart and in habitat

create_pops <- function(n,
												mindist,
												landscape) {
	# create observation window out of landscape
	o_win <- as.owin.SpatialPolygons(rasterToPolygons(
		landscape,
		fun = function(x) {
			x == 1
		},
		dissolve = TRUE
	))
	
	# simulate points (populations) with inhibition distance
	pops <- rSSI(mindist, n, win = o_win)
	
	#convert coords to matrix and return
	coords_matrix <- as.matrix(coords(pops))
	rownames(coords_matrix) <- LETTERS[1:n]
	return(coords_matrix)
	
}


init_pops <- function(n_pops,
											n_ind ,
											sex_ratio,
											n_allels ,
											n_loci,
											n_cov,
											pops) {
	# init population
	sim_pops <- PopGenReport::init.popgensim(n_pops,
																					 n_ind,
																					 sex_ratio,
																					 n_loci,
																					 n_allels,
																					 pops,
																					 n_cov)
	
	return(sim_pops)
}

sim_pops <- function(pops_in,
										 steps,
										 n_offspring,
										 n_ind,
										 mig_rate,
										 disp_max,
										 disp_rate,
										 n_allels,
										 mut_rate,
										 n_cov,
										 landscape,
										 pops,
										 method,
										 NN) {
	# Create a costdistance matrix
	cost_mat <- costdistances(landscape,
														pops,
														method,
														NN)
	
	simpops <- run.popgensim(
		pops_in,
		steps,
		cost_mat,
		n_offspring,
		n_ind,
		mig_rate,
		disp_max,
		disp_rate,
		n_allels,
		mut_rate,
		n_cov,
		rec = "none"
	)
}

simulate_abc <- function(runs, set_res=NULL) {
	summary_stat <- future.apply::future_lapply(
		runs,
		FUN = function(x) {
			if (is.null(set_res)) resistance <- runif(1, 1, 20) else resistance <- set_res 
			#set parameter
			#
			new_landscape <-
				set_resistance(landscape, resistance)
			
			pop_sim <- sim_pops(
				pop_new,
				para$steps,
				para$n_offspring,
				para$n_ind,
				para$mig_rate,
				para$disp_max,
				para$disp_rate,
				para$n_allels,
				para$mut_rate,
				para$n_cov,
				new_landscape,
				pops,
				para$method,
				para$NN
			)
			
			#possible summary statistics
			gi <-
				PopGenReport::pops2genind(pop_sim)
			sum_stat <-
				round(PopGenReport::pairwise.fstb(gi), 3)
			return(list(
				resistance = resistance,
				summary_stat = sum_stat,
				pop = gi
			))
			return(summary_stat)
		})
}
		
