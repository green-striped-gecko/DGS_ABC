# load a bazillion of packages


# devtools::install_github(marcosci/nlmr)
# devtools::install_github(marcosci/landscapetools)

library("secr")
library("raster")
library("nlmr")
library("landscapetools")
library("spatstat")
library("maptools")
library("PopGenReport")
library("tibble")
library("dplyr")
library("magrittr")
library("foreach")
library("doParallel")
library("future")

# when we need to run something on a hpc:
# library("future.batchtools")