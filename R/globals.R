
para <- list(

# Define landscape
n_col           = 100, # number of columns for the landscape ----
n_row           = 100, # number of rows for the landscape ----
init_resistance =   1, # initial value for the resistance ----
ratio           = 0.5, # ratio between matrix & habitat ----

# Define populations (dynamics)
n_pops      =    8,    # number of populations to spawn ----
n_ind       =  100,    # number of individuals to be placed ----
sex_ratio   =  0.5,    # sex ratio in populations ----
n_cov       =    3,    # ? needs to be set to three, I vaguely remember that Bernd said that this will be something for a future update of Popgenreport to handle covariates 
n_offspring =    2,    # number of offspring per ??? ----
mig_rate    =  0.1,    # migration rate per ??? ----
disp_max    =   50,    # average  dispersal of an individual in map units ----
disp_rate   = 0.05,    # proportion of dispersing individuals ----

# Define genetics
n_allels    =    10,       # number of alleles ----
n_loci      =    20,       # number of alleles ----
mut_rate    = 0.001,       # mutation rate ----
method      = "leastcost", #rSPDdistance, commute (explore similarity to circuitscape) ----
NN          = 8,           #number of neighbours for the cost distance method ----
steps       = 100,          # steps for the genetic simulation

# Resistance values to be used in our simulation ----
resistance = 1:20,
rep_resistance = 1:100
)
