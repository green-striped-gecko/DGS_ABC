# source packages, function and parameters
#source("R/packages.R")


library(future.apply)
library("future")
library("listenv")
library("future.batchtools")
library(magrittr)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)
library(maptools)
library(rgeos)

#options(future.makeNodePSOCK.rshcmd = c("plink", "-ssh"))
options(future.makeNodePSOCK.rshopts = c("-i", "d:/bernd/sigs/dnk2.ppk"))



login <- tweak(remote, workers="dungog.win.canberra.edu.au", user="gruber")





# Set up access to remote login node
 login_ssh <- tweak(remote, workers = "dungog.win.canberra.edu.au", user="gruber") # doesn't work because R not installed
#login <- tweak(remote, workers = "gwdu101.gwdg.de", user = 'msciain')
btmc <- tweak(batchtools_multicore)#, template = 'lsf.tmpl', 
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

plan(list(login_ssh, multiprocess))

#set parameters...
source("R/globals.R")

# simulate initial landscape ----
landscape <- nlm_randomcluster(para$n_col,                                      para$n_row, p=para$ratio)

landscape <- landscape -1




# distribute populations across the landscape ----
pops <- create_pops(n = para$n_pops, 
                    mindist = 5,
                    landscape = landscape)

plot(landscape)
points(pops, pch=16,cex=2)

# initialize the populations ----
pop_new <- init_pops(n_pops    =  para$n_pops,
                     n_ind     =  para$n_ind,
                     sex_ratio =  para$sex_ratio,
                     n_allels  =  para$n_allels,
                     n_loci    =  para$n_loci,
                     n_cov     =  para$n_cov,
                     pops      =  pops)


paras <-  expand.grid(rep=1:7)
paras <-  expand.grid(rep=1:10000)

#plan(list( sequential))  #local one core
#to run locally on all cores
plan( multiprocess)  
system.time(y <- simulate_abc(1:nrow(paras)))  

#run remotely on all cores
plan(list(login, multiprocess)) #
y %<-% simulate_abc(1:nrow(paras))
system.time(print(length(y)))

set_resis <- 5
system.time(y.obs <- simulate_abc(1, set_res=set_resis))

library(mmod)
#load("d:/bernd/projects/dgs/2018/y2.rdata")

nn <- length(y)
res <- data.frame(r=rep(NA,nn))
res$pfst<- unlist(future.apply::future_lapply(1:nn, function(x) sum( (as.dist(y[[x]]$summary_stat)-as.dist(y.obs[[1]]$summary_stat))^2)   ))

res$mfst<- unlist(future.apply::future_lapply(1:nn, function(x)  (mean(pairwise.fstb(y[[x]]$pop)) - mean(pairwise.fstb(y.obs[[1]]$pop)) )^2   ))


res$pSp<- unlist(future.apply::future_lapply(1:nn, function(x)  sum((pairwise.propShared(y[[x]]$pop) - pairwise.propShared(y.obs[[1]]$pop))^2) ))


#res$pGst<- unlist(future.apply::future_lapply(1:nn, function(x)  sum(((pairwise_Gst_Nei(y[[x]]$pop)) - pairwise_Gst_Nei(y.obs[[1]]$pop)) ^2)   ))

#res$pGstH<- unlist(future.apply::future_lapply(1:nn, function(x)  sum(((pairwise_Gst_Hedrick(y[[x]]$pop)) - pairwise_Gst_Hedrick(y.obs[[1]]$pop)) ^2)))


#res$indGD <- unlist(future.apply::future_lapply(1:nn, function(x)
#	{
#	gl <-gi2gl((y[[x]]$pop))
#	gl.obs <-gi2gl((y.obs[[1]]$pop))
#	sum( dist(as.matrix(gl)-as.matrix(gl.obs)))
#}))





res$r <- sapply(1:nn, function(x) y[[x]]$resistance)

par(mfrow=c(1,2))

for(i in c(2:3))
{
ss <- res[,i]

th <- quantile(ss,0.001)
plot(density(res[ss<th,1]), main=colnames(res)[i])
#table(round(res[ss<th,2]))

abline(v=set_resis)

}									




