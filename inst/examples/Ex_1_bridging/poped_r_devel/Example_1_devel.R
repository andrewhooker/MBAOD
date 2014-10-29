## RUN MBAOD simulation with missspecified prior. 
## using NONMEM and PopED in R
## also trying to make things less model dependent
## and less NONMEM dependent

## simulated values (the truth) in this case will be c(emax=2,e50=25,hill=5)

#set path to the directory where this file is located
setwd("~/Documents/_PROJECTS/AOD/repos/MBAOD/inst/examples/Ex_1_bridging/poped_r_devel")

# remove things from the global environment
rm(list=ls())

library(PopED)
library(mvtnorm)
## or load the development version ##
#library(devtools); load_all("/Users/ahooker/Documents/_PROJECTS/PopED_in_R/poped_r/PopED/")

library(xpose4)

# load the MBAOD package
source(file.path("..","..","..","tools","sourceDir.R"))
sourceDir(file.path("..","..","..","..","R"),trace=F)

# load the PopED model file
source("PopED_files/poped.mod.PK.1.comp.maturation.R")

step_1=list(
  design = list(
    groupsize = 50,
    a = c(WT=70),
    xt = c(0.1,1,2,4,6,8,24)
  ),
  optimize=NULL,
  simulate=list(target="NONMEM", model="./NONMEM_files/sim.mod",
                data=list(dosing = list(list(AMT=1000,Time=0)),
                          manipulation = list(expression(AMT <- AMT*WT/70))
                )
  ),
  estimate=list(target="NONMEM", model="./NONMEM_files/est_red.mod")
)

step_2 = list(
  design = list(
    groupsize = 20,
    a   = c(WT=35),
    xt = c(0.5,1,2,3,6,12,24)
  ),
  optimize=list(target="poped_R",
                model = list(
                  ff_file="PK.1.comp.maturation.ff",
                  fError_file="feps.add.prop",
                  fg_file="PK.1.comp.maturation.fg"
                ),
                design_space=list(maxa=70,
                                  mina=1,
                                  minxt=0,
                                  maxxt=24),
                parameters=list(
                  bpop=c(EMAX=2,EC50=5,HILL=5), # initial parameter guess not coming from previous step
                  manipulation=list(expression(bpop[1] <- bpop[1]-bpop[3])) # manipulation of initial parameters
                ),
                settings.db=NULL,
                settings.opt=list(
                  opt_xt=T,
                  opt_a=T,
                  bUseRandomSearch= 1,
                  bUseStochasticGradient = 0,
                  bUseBFGSMinimizer = 0,
                  bUseLineSearch = 0,
                  compute_inv=F
                )
  ),
  simulate=list(target="NONMEM", model="./NONMEM_files/sim.mod",
                data=list(
                  dosing = list(list(AMT=1000,Time=0)),
                  manipulation = list(expression(AMT <- AMT*WT/70))
                )
  ),
  estimate=list(target="NONMEM", model="./NONMEM_files/est_full.mod")
)


step_3 <- step_2
step_3$optimize$parameters <- NULL


# source("create.poped.database.R")
# assignInNamespace("create.poped.database",create.poped.database, ns="PopED")


results_all <- mbaod_simulate(cohorts=list(step_1,step_2,step_3), # anything after step_3 is the same as step_3
                              ncohorts=4, # number of steps or cohorts in one AOD
                              rep=2, #number of times to repeat the MBAOD simulation 
                              name="Example_1", 
                              description="4 steps, 1 group per step")



# ## now make some plots
# ## here are some examples
# 
# library(grid)
library(ggplot2)
#library(reshape2)


#################################
######## optimized designs
#################################

#all_designs
design_list <- results_all[grep("^iteration",names(results_all))]

all_designs <- combine_designs(design_list,design_name = "final_design")

model = list(
  ff_file="PK.1.comp.maturation.ff",
  fError_file="feps.add.prop",
  fg_file="PK.1.comp.maturation.fg"
)

parameters_true=list(
  bpop=c(CL=1,V=20,EMAX=2,EC50=25,HILL=5),
  d=c(0.05,0.05),
  sigma=c(0.015,0.0015)
)

poped.db <- do.call(create.poped.database,c(all_designs,model,parameters_true))

plot1 <- plot_model_prediction(poped.db,y_lab="Concentration")
plot1 + theme(legend.position="none")



#################################
######## PARAMETER ESTIMATES 
#################################

true_values <- c(thetas=c(1,20,2,25,5),
                 omegas=sqrt(c(0.05,0.05)),
                 sigmas=sqrt(c(0.015,0.0015)))

plot_parameter_estimates(results_all,true_values)

#################################
######## VPC of IPRED from estimated models and true model
#################################

design_1 = list(
  groupsize = 200,
  m=1,
  a   = 35,
  xt = c(0.5,1,2,3,6,12,24)
)

design_2 = list(
  groupsize = 200,
  m=4,
  a   = rbind(10, 35, 55, 70),
  xt = c(0.5,1,2,3,6,12,24)
)

model = list(
  ff_file="PK.1.comp.maturation.ff",
  fError_file="feps.add.prop",
  fg_file="PK.1.comp.maturation.fg"
)

parameters_true=list(
  bpop=c(CL=1,V=20,EMAX=2,EC50=25,HILL=5),
  d=c(0.05,0.05),
  sigma=c(0.015,0.0015)
)

mbaod_vpc(design_1, 
          model, 
          parameters_true, 
          results_all)

mbaod_vpc(design_2, 
          model, 
          parameters_true, 
          results_all, 
          separate.groups=T)


# #################################
# ######## Clearance plots (specific for this problem) -- visualization of WT choices
# #################################

CL_mod <- function(params=list(BASE=1,EMAX=2,E50=25,HILL=5),IDV){
  with(params,{
    vals <- BASE+ (EMAX*IDV^HILL)/(E50^HILL + IDV^HILL)
    return(vals)
  })
}

df <- data.frame(WT=0:70)
df$CL=CL_mod(IDV=df$WT)

#all_designs
design_list <- results_all[grep("^iteration",names(results_all))]
all_designs <- combine_designs(design_list,design_name = "final_design")

df.2 <- data.frame(all_designs$a)
df.2$CL=CL_mod(IDV=df.2$WT)
nrep <- length(grep("^iteration",names(results_all)))
ncohort <- size(df.2,1)/nrep
df.2$Cohort=as.factor(rep(1:ncohort,nrep))  

p <- ggplot(data=df, aes(x=WT,y=CL))
p <- p+geom_line()
p+geom_point(data=df.2,aes(color=Cohort),size=4)

