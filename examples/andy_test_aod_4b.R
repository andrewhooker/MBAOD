setwd("/Users/ahooker/Documents/_PROJECTS/AOD/repos/aod/")
rm(list=ls())
#source("~/temp/aod2/functions3.R")
source("functions3_andy.R")
source("makeSimFile_andy.R")

########################################################
# 5. HANDLING OF MISSPECIFICATION: ADAPTIVE DESIGN: NUMBER STEPS
#    - Adaptive optimial design
#    - Included previous covariates in design
#    - Vary nr of steps, nr of patients.
#    - Different levels of misspecification
########################################################
#samples=50;
rep=100;
#missp<-list(c(2, 25, 5), c(5, 25, 5), c(.01, 25, 5), c(2, 5, 5), c(2, 50, 5), c(5, 25, 2))
#missp<-list(c(.01, 25, 5),c(2,5,5),c(2,50,5),c(2,25,5))
missp<-list(c(2,25,5))
description="AOD NPATIENTS"
nsteps=4

sampletimes=list(init=list(c(0.5,1,2,3,6,12,24),
                   c(0.5,1,2,3,6,12,24),
                   c(0.5,1,2,3,6,12,24),
                   c(0.5,1,2,3,6,12,24)))

cov=list(init=c(1, 1, 1, 1),
  min=c(1, 1, 1, 1),
  max=c(70, 70, 70, 70))

#for(nid in c(5)){

  nid=c(7,7,6)
  ngroup=3
#  cur.groupsize=list(init=rep(nid,ngroup),
#    min=rep(nid,ngroup),
#    max=rep(nid,ngroup))
  cur.groupsize=list(init=nid,
    min=nid,
    max=nid)

  cur.samples=list(init=sampletimes$init[1:ngroup],
    min=sampletimes$init[1:ngroup], #this seems wrong
    max=sampletimes$init[1:ngroup]) # this seems wrong

  cur.cov=list(init=cov$init[1:ngroup],
    min=cov$min[1:ngroup],
    max=cov$max[1:ngroup])     

  #count=1
  
  for(m in missp){
    #name=paste("2_",count,sep="")
    name=paste("5_nsteps_",nsteps,"_nid_",paste(nid,collapse="_"),"_rep_",rep,"_missp_",paste(m, collapse="-"),sep="")

  if(file.exists(paste("resultI_",name,".Rdata",sep=""))){
    next()
  }
  
    runRepAna(name=name, rep=rep,
              nsteps=nsteps,
              prev=list(list(ngroups       = 1, 
                dose          = list(1000),     # necessery for makeDataSim
                nid           = 50, # necessery for makeDataSime
                cur.groupsize = list(init=c(50), min=c(50),  max=c(50)),
                dataset       = NULL,
                cov           = 70,
                samplingtimes = list(c(0,1,2,4,6,8,24)),
                ofv           = NA)),
              models = list(modfullest="run2fE.mod", modfullsim="run2fS.mod", modredest="run3rE.mod", modredsim="run2rS.mod"),
              cur.cov=cur.cov,
              unfix=list(theta = "1 1 1 1 1", omega="1 1", sigma="1 1", parDsUnInteresting = "0 0 0 0 0 0 0 0 0"),
              cur.groupsize=cur.groupsize,
              samples=cur.samples,
              settings=list(remote = FALSE,
                optimizedSamplingtimes=TRUE, optimizedCovariates=TRUE,
                ODtype="D", ODcalctype="detFIM", # xD or xDs
                runRemote=FALSE,
                poped.sh.script="run_andy.sh",
                poped.cluster=FALSE,
                sse.remove.folder=FALSE, 
                sse.clean=1,
                sse.run.on.sge=FALSE),
              mpar=m, fixed=FALSE,
              description=description
              )
    #summary<-summaryResults(name=name)   
    #evalDesign(name=name, summary=summary, description=description,
    #           samples=50,#samples,
    #           ngroups=nsteps, nsteps=nsteps, misspec=m)
    #count=count+1
  }
#}
