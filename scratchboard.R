# source packages, function and parameters
source("R/packages.R")
source("R/functions_landscape.R")
source("R/functions_population.R")
source("R/globals.R")


# simulate initial landscape ----
landscape <- simulate_landscape_discrete(para$n_col,
                                         para$n_row,
                                         germs = 100)

# distribute populations across the landscape ----
pops <- create_pops(para$n_pops, 
                    2,
                    landscape)

# initialize the populations ----
pop_new <- init_pops(n_pops    =  para$n_pops,
                     n_ind     =  para$n_ind,
                     sex_ratio =  para$sex_ratio,
                     n_allels  =  para$n_allels,
                     n_loci    =  para$n_loci,
                     n_cov     =  para$n_cov,
                     pops      =  pops)


# plan(batchtools_lsf)
plan(multisession)  
y <- future_lapply(para$resistance,
                   FUN =  simulate_abc,
                   rep_resistance = para$rep_resistance)