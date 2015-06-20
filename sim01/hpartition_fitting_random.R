if(!exists('MAXOVERLAP')) MAXOVERLAP = '001'
if(!exists('NSIM')) NSIM = '050'
if(!exists('NDATA')) NDATA = '200'
if(!exists('K0')) K0 = '003'
if(!exists('Kf')) Kf = '009'
if(!exists('DIM')) DIM = '005'
if(!exists('SEED')) SEED = '001'

IFILE = sprintf("data/d-MAXOVERLAP_%s-NSIM_%s-NDATA_%s-K0_%s-Kf_%s-DIM_%s-SEED_%s.RData", 
                MAXOVERLAP, NSIM, NDATA, K0, Kf, DIM, SEED)
load(IFILE)

suppressMessages( library(mixpack) )

# source('sim01/models.R')
# lambda = l_lambda[['demp']]
# omega = l_omega[['prop']]
# load('sim01/data/d-MAXOVERLAP_019-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_005-SEED_001.RData')

HP = lapply(simulation, function(sim){
  LEVEL = max(sim$g)
  tm = system.time( hp <- get_random_hierarchical_partition(K = ncol(sim$tau)) )
  list(
    'time' = tm,
    'hp_clust' = cluster_partition(tau = sim$tau, partition = hp[[LEVEL]]),
    'g' = sim$g )
})