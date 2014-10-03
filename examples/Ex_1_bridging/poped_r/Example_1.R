## RUN MBAOD simulation with missspecified prior. 
## using NONMEM and PopED in R
## also trying to make things less model dependent
## and less NONMEM dependent

## simulated values (the truth) in this case will be c(emax=2,e50=25,hill=5)

#set path to the directory where this file is located
setwd("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/examples/Ex_1_bridging/poped_r")

# remove things from the global environment
rm(list=ls())

library(PopED)

# load the MBAOD package
source("../../../tools/sourceDir.R")

sourceDir("../../../R",trace=F)

# load the PopED model file
source("PopED_files/poped.mod.PK.1.comp.maturation.R")



runRepAna(name="Example_1_Results", 
          rep=100, #number of times to repeat the MBAOD simulation 
          nsteps=4, # number of steps or cohorts in one AOD
          prev=list(list(ngroups       = 1, 
                         dose          = list(1000),     # necessery for makeDataSim
                         nid           = 50, # necessery for makeDataSime
                         cur.groupsize = list(init=c(50), min=c(50),  max=c(50)),
                         dataset       = NULL,
                         cov           = 70,
                         samplingtimes = list(c(0,1,2,4,6,8,24)),
                         ofv           = NA)),
          models = list(modfullest="./NONMEM_files/run2fE.mod", 
                        modfullsim="./NONMEM_files/run2fS.mod", 
                        modredest="./NONMEM_files/run3rE.mod", 
                        modredsim="./NONMEM_files/run2rS.mod"),
          cur.cov=list(init=c(1), 
                       min=c(1),  
                       max=c(70)
          ),
          unfix=list(theta = "1 1 1 1 1", omega="1 1", sigma="1 1", parDsUnInteresting = "0 0 0 0 0 0 0 0 0"),
          cur.groupsize=list(init=20, min=20,  max=20),
          samples=list(init=list(c(0.5,1,2,3,6,12,24))),
          settings=list(remote = FALSE,
                        optimizedSamplingtimes=TRUE, 
                        optimizedCovariates=TRUE,
                        ODtype="D", ODcalctype="detFIM", # xD or xDs
                        runRemote=FALSE,
                        poped.sh.script="../scripts/run_andy.sh",
                        poped.cluster=FALSE,
                        sse.remove.folder=TRUE, 
                        sse.clean=3,
                        sse.run.on.sge=FALSE,
                        ff_file="PK.1.comp.maturation.ff",
                        fError_file="feps.add.prop",
                        fg_file="PK.1.comp.maturation.fg",
                        trflag=FALSE), # good if we don't want to take inverses of poorly defined matricies in OD calcs
          mpar=c(EMAX=2,EC50=5,HILL=5), # initial parameter guess
          fixed=FALSE,
          description="AOD_test_2",
          overwrite=T,
          OD_tool="poped_R"
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
