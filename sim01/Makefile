MAXOVERLAP = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50

DGEN = $(foreach v,$(MAXOVERLAP),$(shell printf 'data/d-MAXOVERLAP_%03d-NSIM_050-NDATA_200-K0_003-Kf_009-DIM_005-SEED_001.RData' $(v)))

all : $(DGEN)

data/d-%.RData : build_data.R
	Rscript -e 'sapply(strsplit(strsplit("$*","-")[[1]],"_"),function(v,e)assign(v[1],as.numeric(v[2]),e),environment()); source("$<")'
