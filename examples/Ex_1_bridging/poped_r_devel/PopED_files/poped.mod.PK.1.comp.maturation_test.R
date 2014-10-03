library(PopED)

## set working directory to the location of this file 
setwd("~/Documents/_PROJECTS/AOD/repos/MBAOD/examples/Ex_1_bridging/poped_r_devel/PopED_files")

source("poped.mod.PK.1.comp.maturation.R")

bpop_vals <- c(CL=1.8,V=20,EMAX=2,EC50=25,HILL=5)
d_vals <- c(CL=0.05,V=0.05)

# -- Matrix defining the variances of the residual variability terms --
sigma_vals <- diag(c(0.015,0.0015))

# initial design
poped.db.1 <- create.poped.database(ff_file="PK.1.comp.maturation.ff",
                                    fError_file="feps.add.prop",
                                    fg_file="PK.1.comp.maturation.fg",
                                    groupsize=rbind(50,20,20,20),
                                    m=4,
                                    sigma=sigma_vals,
                                    bpop=bpop_vals, 
                                    d=d_vals, 
                                    xt=c( 1,2,4,6,8,24),
                                    minxt=0,
                                    maxxt=24,
                                    a=rbind(70,60,50,10),
                                    maxa=70,
                                    mina=1)


## Choose database
poped.db <- poped.db.1 # original model and design

##  create plot of model 
plot_model_prediction(poped.db)
plot_model_prediction(poped.db,IPRED=T,DV=T,separate.groups=T)


## evaluate initial design
FIM <- evaluate.fim(poped.db) 
FIM
det(FIM)
get_rse(FIM,poped.db)

# RS+SG+LS optimization of sample times
output <- poped_optimize(poped.db,opt_xt=T,opt_a=T)

# MFEA optimization with only integer times allowed
 mfea.output <- poped_optimize(poped.db,opt_a=1,opt_xt=0,
                               bUseExchangeAlgorithm=1,
                               EAStepSize=1)
plot_model_prediction(mfea.output$poped.db,IPRED=T,DV=T,separate.groups=F)

