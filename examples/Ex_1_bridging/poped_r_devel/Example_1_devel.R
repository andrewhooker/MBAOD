## RUN MBAOD simulation with missspecified prior. 
## using NONMEM and PopED in R
## also trying to make things less model dependent
## and less NONMEM dependent

## simulated values (the truth) in this case will be c(emax=2,e50=25,hill=5)

#set path to the directory where this file is located
setwd("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/examples/Example_1_poped_r")

# remove things from the global environment
rm(list=ls())

library(PopED)

# load the MBAOD package
source("../../tools/sourceDir.R")
sourceDir("../../R",trace=F)

# source the files used for the example
source("mbaod_simulate.R")
source("model_prediction.R")
source("check_design.R")

# load the PopED model file
source("PopED_files/poped.mod.PK.1.comp.maturation.R")

step_1=list(
  design = list(
    groupsize = 50,
    a = c(WT=70),
    xt = c(0,1,2,4,6,8,24)
  ),
  optimize=NULL,
  simulate=list(target="NONMEM", model="./NONMEM_files/run2rS.mod",
                dosing = list(list(AMT=1000,Time=0))),
  estimate=list(target="NONMEM", model="./NONMEM_files/run3rE.mod")
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
                mpar=c(EMAX=2,EC50=5,HILL=5), # initial parameter guess not coming from previous step
                optimizedSamplingtimes=TRUE, 
                optimizedCovariates=TRUE,
                ODtype="D", 
                ODcalctype="detFIM"
  ),
  simulate=list(target="NONMEM", model="./NONMEM_files/run2fS.mod",
                dosing = list(list(AMT=1000,Time=0))),
  estimate=list(target="NONMEM", model="./NONMEM_files/run2fE.mod")
)

step_3 <- step_2
step_3$optimize$mpar <- NULL




# source("create.poped.database.R")
# assignInNamespace("create.poped.database",create.poped.database, ns="PopED")


mbaod_simulate(cohorts=list(step_1,step_2,step_3), # anything after step_3 is the same as step_3
               ncohorts=1, # number of steps or cohorts in one AOD
               AOD_rules = list(keep_data=TRUE,update_parameters=TRUE), 
               rep=1, #number of times to repeat the MBAOD simulation 
               name="Example_1", 
               description="Example 1: 4 steps, 1 group per step"
)


# ## now make some plots
# ## here are some examples
# 
# library(grid)
# library(ggplot2)
# library(reshape2)
# 
# #################################
# ######## PARAMETER ESTIMATES 
# #################################
# 
# summary.aod <- summaryResults(name=name)   
# 
# ree.aod <- extract.ree(summary.aod)
# stack.aod <- stack(ree.aod,select=c(thCl,thV,thMax,thE50,thHill))
# #stack.aod <- stack(ree.aod,select=c(thCl,thV,thMax,thE50,thHill,omCL,omV,sigP,sigA))
# stack.aod$type <- "AOD: 1 group per cohort, 4 cohorts"
# 
# # could merge multiple results here:
# #foo <- rbind(stack.aod,stack.fix)
# 
# p <- ggplot(data=stack.aod,aes(x=ind,y=values))
# p +  geom_boxplot() + geom_jitter(position=position_jitter(width=0.05)) + facet_grid(~type) + 
#   #scale_y_continuous(name="REE") + 
#   coord_cartesian(ylim = c(-100, 100)) + 
#   #scale_x_discrete(name="Parameter") +
#   xlab("Parameter") +
#   ylab("REE (%)") +
#   theme(title = element_text(size=25),
#         axis.title = element_text(size=20),
#         axis.text = element_text(size=20),
#         strip.text=element_text(size=20))
# 
# 
# #################################
# ######## AUC plots
# #################################
# 
# final.par.aod <- summary.aod[[2]][!duplicated(summary.aod[[2]]$rep,fromLast=T),]
# 
# auc.aod <- compute.auc(final.par.aod)
# 
# ##auc.vals <- auc.aod
# ##quantile(auc.vals[auc.vals$wt==1,]$obsAUC,probs=c(0.05,0.25,0.5,0.75,0.95))
# 
# auc.aod$type <- "AOD"
# 
# auc.aod$type <- "AOD: 1 group per cohort"
# 
# # auc.both <- rbind(auc.fix,auc.aod) 
# auc.both <- rbind(auc.aod) 
# 
# p <- ggplot(data=auc.both)
# p + geom_line(aes(x=wt,y=trueAUC)) + geom_point(aes(x=wt,y=obsAUC),alpha=0.5) + scale_y_continuous(name="AUC") + facet_grid(~type)
# 
# p + geom_line(aes(x=wt,y=trueAUC),col="blue") + 
#   #geom_point(aes(x=wt,y=obsAUC),alpha=0.05)+
#   stat_summary(aes(x=wt,y=obsAUC),geom="ribbon",fun.data="median_hilow",alpha=0.3, fill="red")+
#   scale_y_continuous(name="AUC") + facet_grid(~type) +
#   theme(title = element_text(size=25),
#         axis.title = element_text(size=20),
#         axis.text = element_text(size=20),
#         strip.text=element_text(size=20))
# 
# 
# #################################
# ######## CL plots
# #################################
# 
# em.curve <- function(params=c(1,2,25,5),wt=wt){
#   emax.curve <- params[[1]]+ (params[[2]] * wt^params[[4]])/(params[[3]]^params[[4]] + wt^params[[4]])
#   return(emax.curve)
# }
# 
# pars.2 <- subset(summary.aod[[2]],grp==2 & rep==1)
# pars.3 <- subset(summary.aod[[2]],grp==3 & rep==1)
# pars.4 <- subset(summary.aod[[2]],grp==4 & rep==1)
# 
# params.2 <- with(pars.2,c(thCl,thMax,thE50,thHill))
# params.3 <- with(pars.3,c(thCl,thMax,thE50,thHill))
# params.4 <- with(pars.4,c(thCl,thMax,thE50,thHill))
# 
# emax.curve$tureEMAX <- unlist(lapply(1:70,"em.curve",params=c(1,2,25,5)))
# emax.curve$em.co.1 <- unlist(lapply(1:70,"em.curve",params=c(1,2,5,5)))
# emax.curve$em.co.2 <- unlist(lapply(1:70,"em.curve",params=params.2))
# emax.curve$em.co.3 <- unlist(lapply(1:70,"em.curve",params=params.3))
# emax.curve$em.co.4 <- unlist(lapply(1:70,"em.curve",params=params.4))
# 
# emax.curve$COV1 <- NA
# emax.curve$COV1[1] <- 70
# emax.curve$COV1.cl <- em.curve(wt=emax.curve$COV1,params=c(1,2,5,5))
# 
# emax.curve$COV2 <- NA
# emax.curve$COV2[1] <- summary.aod[[1]][2,]$cov
# emax.curve$COV2.cl <- em.curve(wt=emax.curve$COV2,params=c(1,2,5,5))
# 
# emax.curve$COV3 <- NA
# emax.curve$COV3[1] <- summary.aod[[1]][3,]$cov
# emax.curve$COV3.cl <- em.curve(wt=emax.curve$COV3,params=params.2)
# 
# emax.curve$COV4 <- NA
# emax.curve$COV4[1] <- summary.aod[[1]][4,]$cov
# emax.curve$COV4.cl <- em.curve(wt=emax.curve$COV4,params=params.3)
# 
# p3 <- ggplot(data=emax.curve) +
#   geom_line(aes(x=wt,y=trueEMAX),col="blue") +
#   geom_line(aes(x=wt,y=em.co.1),col="red") +
#   geom_line(aes(x=wt,y=em.co.2),col="green") +
#   geom_line(aes(x=wt,y=em.co.3),col="orange") +
#   geom_line(aes(x=wt,y=em.co.4),col="purple") +
#   geom_point(aes(x=COV1,y=COV1.cl),cex=5,col="red") +
#   geom_point(aes(x=COV2,y=COV2.cl),cex=5,col="red") +
#   geom_point(aes(x=COV3,y=COV3.cl),cex=5,col="green") +
#   geom_point(aes(x=COV4,y=COV4.cl),cex=5,col="orange") +  
#   xlab("WT") +
#   ylab("Clearance")+
#   theme(axis.title = element_text(size=20),
#         title = element_text(size=25),
#         axis.text = element_text(size=20))
# p3
# 
# 
