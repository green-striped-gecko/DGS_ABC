library(ResistanceGA)



asc <- landscape
plot(asc)

#locale <- samples
sp <- SpatialPoints(pops)


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
gd <-c(as.dist(pairwise.fstb(y[[1]]$pop))) 
plot(true.dist ~ gd)

plot(landscape)
plot(sp, add = T)

gdist.inputs <- gdist.prep(n.Pops = length(sp),
													 method = "costDistance",
													 samples = sp,
													 response = gd
)

ss.results <- SS_optim(gdist.inputs = gdist.inputs,
											 GA.inputs = GA.inputs)

ss.results$CategoricalResults
