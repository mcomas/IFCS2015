suppressMessages( library(mixpack) )

# source('sim01/models.R')
# lambda = l_lambda[['demp']]
# omega = l_omega[['prop']]
# load('sim01/data/d-MAXOVERLAP_019-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_005-SEED_001.RData')

HP = lapply(simulation, function(sim){
  LEVEL = max(sim$g)
  tm = system.time( hp <- get_hierarchical_partition(tau = sim$tau, omega = omega, lambda = lambda) )
  list(
    'time' = tm,
    'hp_clust' = cluster_partition(tau = sim$tau, partition = hp[[LEVEL]]),
    'g' = sim$g )
})