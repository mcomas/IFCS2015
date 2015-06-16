if(!exists('MAXOVERLAP')) MAXOVERLAP = 1
if(!exists('NSIM')) NSIM = 50
if(!exists('NDATA')) NDATA = 200
if(!exists('K0')) K0 = 3
if(!exists('Kf')) Kf = 9
if(!exists('DIM')) DIM = 5
if(!exists('SEED')) SEED = 78

OFILE = sprintf("data/d-MAXOVERLAP_%03d-NSIM_%03d-NDATA_%03d-K0_%03d-Kf_%03d-DIM_%03d-SEED_%03d.RData", 
                MAXOVERLAP, NSIM, NDATA, K0, Kf, DIM, SEED)

set.seed(SEED)

suppressMessages( library(MixSim) )
suppressMessages( library(Rmixmod) )

simulation = lapply(1:NSIM, function(i){
  ms = MixSim(MaxOmega=MAXOVERLAP/100, K=K0, p=DIM, PiLow=0.01, sph=TRUE)
  sim = simdataset(n=NDATA, Pi=ms$Pi, Mu=ms$Mu, S=ms$S)
  repeat{
    suppressWarnings(
      tau <- mixmodCluster(data.frame(sim$X), 
                           nbCluster=Kf, 
                           models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_I"))@bestResult@proba)
    if(dim(tau)[1] != 0) break
  }
  tau[tau == 0] = .Machine$double.xmin  
  list('data' = sim$X, 'g' = sim$id, 'tau' = tau)
})

save(simulation, file=OFILE)