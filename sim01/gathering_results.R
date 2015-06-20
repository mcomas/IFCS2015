library(dplyr)

source("models.R")

lambda = names(l_lambda)
omega = names(l_omega)

ptrn_meth = 'data/h-lambda_%s-omega_%s-MAXOVERLAP_%03d-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_010-SEED_001.RData'
ptrn_rnd = 'data/h-random-MAXOVERLAP_%03d-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_010-SEED_001.RData'
suppressWarnings(
  res <- lapply(names(l_lambda), function(l){
    lapply(names(l_omega), function(o, l){
      lapply(1:50, function(mx, o, l){
        load(sprintf(ptrn_meth, l, o, mx))
        meth = Reduce(`rbind`, lapply(HP, function(hp) unlist(MixSim::RandIndex(hp$hp_clust, hp$g)))) %>% 
              data.frame %>% mutate(
                overlap = mx,
                omega = o,
                lambda = l,
                method = 'method')
        load(sprintf(ptrn_rnd, mx))
        rnd = Reduce(`rbind`, lapply(HP, function(hp) unlist(MixSim::RandIndex(hp$hp_clust, hp$g)))) %>% 
              data.frame %>% mutate(
                overlap = mx,
                omega = o,
                lambda = l,
                method = 'random')
        rbind(meth, rnd)
      }, o, l) %>% bind_rows
    }, l) %>% bind_rows
  }) %>% bind_rows
)

save(res, file='data/res_NSIM_050-NDATA_200-K0_003-Kf_009-DIM_010-SEED_001.RData')



library(ggplot2)
res.mean = res %>% group_by(omega, lambda, overlap, method) %>% summarise(AR = mean(AR))
ggplot() + geom_line(data = res.mean, aes(x = overlap, y = AR, col = method)) + 
  facet_grid(omega~lambda) + theme_bw()
