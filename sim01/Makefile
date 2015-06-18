MAXOVERLAP = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50

DGEN = $(foreach v,$(MAXOVERLAP),$(shell printf 'data/d-MAXOVERLAP_%03d-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_005-SEED_001.RData' $(v)))

HGEN_AAAA_BBBB = $(foreach v,$(MAXOVERLAP),$(shell printf 'data/h-lambda_AAAA-omega_BBBB-MAXOVERLAP_%03d-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_005-SEED_001.RData' $(v)))

HGEN_AAAA = $(subst BBBB,cnst,$(HGEN_AAAA_BBBB)) $(subst BBBB,prop,$(HGEN_AAAA_BBBB)) $(subst BBBB,dich,$(HGEN_AAAA_BBBB))

HGEN = $(subst AAAA,entr,$(HGEN_AAAA)) $(subst AAAA,demp,$(HGEN_AAAA)) $(subst AAAA,demp.mod,$(HGEN_AAAA)) \
       $(subst AAAA,coda,$(HGEN_AAAA)) $(subst AAAA,coda.norm,$(HGEN_AAAA)) $(subst AAAA,prop,$(HGEN_AAAA))

all : $(DGEN) $(HGEN)

data/d-%.RData : build_data.R
	Rscript -e 'sapply(strsplit(strsplit("$*","-")[[1]],"_"),function(v,e)assign(v[1],as.numeric(v[2]),e),environment()); source("$<")'

data/h-%.RData : $(DGEN) hpartition_fitting.R models.R 
	Rscript -e 'sapply(strsplit(strsplit("$*","-")[[1]],"_"),function(v,e)assign(v[1],v[2],e),environment()); source("models.R"); lambda = l_lambda[[lambda]]; omega = l_omega[[omega]]; source("hpartition_fitting.R"); save(HP, file="$@")'
