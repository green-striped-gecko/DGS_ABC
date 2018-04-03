set_resistance <- function(landscape, resistance){

    values(landscape)[values(landscape) > 0] <- resistance
    landscape
}
##' Simulate a given population with an updated landscape
##'
##' @title Simulation on updated landscape
##' @param pop population
##' @param resistance
##' @param rep number of runs for a given value of resistance
##' @return list of genind objects and the resistance value
##' @author Sangeeta Bhatia
##' @imports PopGenReport
simulate_abc <- function(pop, landscape, resistance, rep, ...) {

    new_landscape <- set_resistance(landscape, resistance)

    plan(multisession)
    pops_updated <- future_lapply(seq_len(rep),
                                  FUN = function(x) {
                                      pop_sim <- sim_pops(pop,
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
                                                          para$NN)

                                                      pops2genind(pop_sim)})

    list(resistance = resistance, pops_updated = pops_updated)

}

##' Summary statistics for list of simulated populations
##'
##' @title Summary statistics for list of simulated populations and
##' corresponding resistance values
##' @param pops list of genind objects
##' @return list of summary statistics and corresponding resistance
##' value
##' @author Sangeeta Bhatia
##' @imports PopGenReport
summary_stats <- function(pops, FUN){
    fun <- match.fun(FUN)
    plan(multisession)
    summary_stat <- future_lapply(pops, function(x){
                                           fun(x$pops_updated)})
    list(resistance = pops$resistance, summary_stat = summary_stat)

}
