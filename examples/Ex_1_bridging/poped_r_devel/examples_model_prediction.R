## Warfarin example from software comparison in:
## Nyberg et al., "Methods and software tools for design evaluation 
##   for population pharmacokinetics-pharmacodynamics studies", 
##   Br. J. Clin. Pharm., 2014. 

library(PopED)

# temp stuff
library(mvtnorm)
source("model_prediction.R")
source("check_design.R")


## -- parameter definition function 
## -- names match parameters in function ff
sfg <- function(x,a,bpop,b,bocc){
  parameters=c(CL=bpop[1]*exp(b[1]),
               V=bpop[2]*exp(b[2]),
               KA=bpop[3]*exp(b[3]),
               Favail=bpop[4],
               DOSE=a[1])
    return(parameters) 
}

## -- Define initial design  and design space
poped.db <- create.poped.database(ff_file="ff.PK.1.comp.oral.sd.CL",
                                  fg_file="sfg",
                                  fError_file="feps.prop",
                                  bpop=c(CL=0.15, V=8, KA=1.0, Favail=1), 
                                  notfixed_bpop=c(1,1,1,0),
                                  d=c(CL=0.07, V=0.02, KA=0.6), 
                                  sigma=0.01,
                                  groupsize=32,
                                  xt=c( 0.5,1,2,6,24,36,72,120),
                                  minxt=0,
                                  maxxt=120,
                                  a=70)

## data frame with model predictions
model_prediction(poped.db)

##  data frame with with variability 
model_prediction(poped.db,DV=TRUE)

model_prediction(poped.db,include_a = T,include_x = T)
model_prediction(poped.db,include_a = T,include_x = T,DV=T)

model_prediction(poped.db,include_a = T,include_x = T,DV=T,predictions=F)

## -- 2 groups
poped.db.2 <- create.poped.database(ff_file="ff.PK.1.comp.oral.sd.CL",
                                  fg_file="sfg",
                                  fError_file="feps.prop",
                                  bpop=c(CL=0.15, V=8, KA=1.0, Favail=1), 
                                  notfixed_bpop=c(1,1,1,0),
                                  d=c(CL=0.07, V=0.02, KA=0.6), 
                                  sigma=0.01,
                                  groupsize=rbind(3,3),
                                  m=2,
                                  xt=c( 0.5,1,2,6,24,36,72,120),
                                  minxt=0,
                                  maxxt=120,
                                  a=70)

model_prediction(poped.db.2,include_a = T,include_x = T,DV=T)

## without a poped.db, just describing the design
design_1 <- list(
  xt=c( 0.5,1,2,6,24,36,72,120),
  m=2,
  groupsize=3)

design_2 <- list(
  xt=c( 0.5,1,2,6,24,36,72,120),
  m=2,
  groupsize=3,
  a=c(WT=70,AGE=50))

design_3 <- list(
  xt=c( 0.5,1,2,6,24,36,72,120),
  m=2,
  groupsize=3,
  a=list(c(WT=70,AGE=50),c(AGE=45,WT=60)))

model_prediction()
model_prediction(poped.db=NULL,design=design_1,predictions=F)
model_prediction(poped.db=NULL,design=design_2,predictions=F,include_a = T,include_x = T)
model_prediction(poped.db=NULL,design=design_3, predictions=F,include_a = T,include_x = T)
model_prediction(poped.db=NULL,design=design_3,predictions=F,include_a = T,include_x = T,DV=T)

model_prediction(design=design_1)
model_prediction(design=design_2)
model_prediction(design=design_3)
model_prediction(design=design_3,DV=T)

dosing_1 <- list(list(AMT=1000,RATE=NA,Time=0.5),list(AMT=1000,RATE=NA,Time=0.5))
dosing_2 <- list(list(AMT=1000,RATE=NA,Time=0.5),list(AMT=1000,RATE=NA,Time=0.5),list(AMT=1000,RATE=NA,Time=0.5))
dosing_3 <- list(list(AMT=1000,RATE=NA,Time=0.5))
dosing_4 <- list(list(AMT=1000,Time=0.5))
dosing_5 <- list(list(AMT=c(1000,20),Time=c(0.5,10))) # multiple dosing


model_prediction(design=design_3,DV=T,dosing=dosing_1)
model_prediction(design=design_3,DV=T,dosing=dosing_2) # should give a size error
model_prediction(design=design_3,DV=T,dosing=dosing_3)
model_prediction(design=design_3,DV=T,dosing=dosing_4)
model_prediction(design=design_3,DV=T,dosing=dosing_5)


model_prediction(design=design_3,DV=T,dosing=dosing_5,filename="test.csv")


