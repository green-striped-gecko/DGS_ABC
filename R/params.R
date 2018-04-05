## Landscape parameters

  nx <- 50
  ny <- 50
  p <- 0.5      # fragmentation
  A <- 0.5      # expected proportion of habitat
  res <- 10     # resistance value

## pop dynamics parameters

  n_pops <- 8
  n_ind <- 100
  sex_ratio <- 0.5
  n_offspring <- 2
  mig_rate <- 0.1
  disp_max <- 50
  disp_rate <- 0.05

## genetics params

  n_allels <- 10
  n_loci <- 20
  mut_rate <- 0.001
  method <- "leastcost"
  NN <- 8

## simulation params

resistance <- 1:20
num_sims   <- 100
