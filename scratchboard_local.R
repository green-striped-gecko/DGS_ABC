# source packages, function and parameters
source("R/packages.R")
source("R/functions_landscape.R")
source("R/functions_population.R")
source("R/globals.R")

# Set up access to remote login node
# login_vpn <- tweak(remote, workers = "msciain@login.gwdg.de") # doesn't work because R not installed
#login <- tweak(remote, workers = "gwdu101.gwdg.de", user = 'msciain')
#bsub <- tweak(batchtools_lsf, template = 'lsf.tmpl', 
              # workers = "export LSF_ENVDIR=/opt/lsf/conf",
#              resources = list(job.name = 'test1',
#                               log.file = 'landgen.log',
#                               queue = 'mpi-long',
#                               walltime = '120:00',
#                               processes = 24))

## Specify future topology
## login node -> { cluster nodes } -> { multiple cores }
#plan(list(
#    login,
#    bsub,
#    multiprocess
#))

plan(list(multiprocess, multiprocess, multiprocess))

# simulate initial landscape ----
landscape <- nlm_randomcluster(para$n_col,                                      para$n_row, p=0.5)

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



paras <-  expand.grid(rep=1:50)


y <- simulate_abc(1:nrow(paras))

y.obs <- simulate_abc(1)

res <- data.frame(ss= rep(NA,50), r=rep(NA,50))
res$ss<- sapply(1:50, function(x) sum(as.dist(y[[x]]$summary_stat)-as.dist(y.obs[[1]]$summary_stat)^2)   )
res$r <- sapply(1:50, function(x) y[[x]]$resistance)



									
