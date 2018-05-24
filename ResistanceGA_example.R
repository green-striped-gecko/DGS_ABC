library(ResistanceGA)




plot(landscape)

#locale <- samples
sp <- SpatialPoints(pops)
points(sp)

GA.inputs <- GA.prep(ASCII.dir = landscape,
										 Results.dir = "D:/Temp/ResGA/",
										 min.cat = 1,
										 max.cat = 100,
										 method = "LL",
										 parallel = 5,
										 run = 10
)

#asc.feat <- (landscapae * 9) + 1


gdist.inputs <- gdist.prep(n.Pops = length(sp),
													 method = "costDistance",
													 samples = sp
)

#Sim data
#true.dist <- 
gd <-c(as.dist(pairwise.fstb(y.obs[[1]]$pop))) 
cd <- c(as.dist(costdistances(landscape, pops, method = "leastcost",NN = 8)))
plot(gd ~ cd)

plot(landscape)
plot(sp, add = T)

gdist.inputs <- gdist.prep(n.Pops = length(sp),
													 method = "costDistance",
													 samples = sp,
													 response = gd,
)

ss.results <- SS_optim(gdist.inputs = gdist.inputs,
											 GA.inputs = GA.inputs, )

ss.results$CategoricalResults
