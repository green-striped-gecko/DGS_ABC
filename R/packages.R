# load a bazillion of packages


# devtools::install_github(marcosci/nlmr)
# devtools::install_github(marcosci/landscapetools)

library("secr")
library("raster")
#library("NLMR")
library("landscapetools")
library("spatstat")
library("maptools")
library("PopGenReport")
library("tibble")
library("dplyr")
library("magrittr")
library("future")
library("listenv")
library("future.batchtools")
library(magrittr)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)


# when we need to run something on a hpc:
# library("future.batchtools")
