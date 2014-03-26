#setwd("~/temp/aod2")
#rm(list=ls())
source("functions3_andy.R")

########################################################
# 5. HANDLING OF MISSPECIFICATION: ADAPTIVE DESIGN: NUMBER STEPS
#    - Adaptive optimial design
#    - Included previous covariates in design
#    - Vary nr of steps, nr of patients.
#    - Different levels of misspecification
########################################################
#rep=25;
samples=50
missp<-list(c(.01, 25, 5))
description="AOD NPATIENTS"
nid=20
nsteps=4
name=paste("5new_nsteps_",nsteps,"_nid_",nid,"_missp_",paste(missp[[1]], collapse="-"),sep="")
  
files <- dir(pattern="\\.m$")
files <- c(files,dir(pattern="\\.mod$"))
files <- c(files,dir(pattern="\\.sh$"))
files
           
# remove old folder
if(file.exists(name)){
  system(paste('rm -r', name, sep=" "))
}

dir.create(name)
file.copy(files, name, overwrite=TRUE)
setwd(paste("./",name,sep=""))

##################
# add stuff here
##################

# Cleanup   
cleanup()

# define models for simulation and estimation
# then Copy model files to default names 
models = list(modfullest="run2fE.mod", modfullsim="run2fS.mod", modredest="run3rE.mod", modredsim="run2rS.mod")
copyModels(models)

# initial design
prev=list(list(ngroups       = 1,
               dose          = list(1000),   
               nid           = 50, 
               cur.groupsize = list(init=c(50), min=c(50),  max=c(50)),
               dataset       = NULL,
               cov           = 70,
               samplingtimes = list(c(0,1,2,4,6,8,24)),
               ofv           = NA))

# Run adaptive thing
#for(i in 1:nsteps){

i=1 # for testing

##################
#optimize the design
# if the first step then don't optimize
##################
if(i!=1){
  # Create function_input file     
  createFunctionInput(prev     = prev,                # previous run info
                      cur.cov  = cur.cov,             # current init/min/max cov values
                      cur.groupsize = cur.groupsize, # current init/min/max group size
                      unfix    = unfix,                 # fixed / Ds parameters
                      samples  = samples,             # current times + number
                      settings = settings,           # optimization settings
                      simple   = ifelse(i==1, TRUE, FALSE),   # 1st step use prespecified pars
                      mpar     = mpar  )                 # prespecified parameters
  
  # Run PopED
  runPoped(remote = settings$runRemote, name=name)
}
##################
# end optimize the design
##################

# Generate input dataset for SSE
makeSimFile(prev = prev, step=i, last=FALSE)

# Generate simulation and re-estimation (est: including previous runs)
runSSE(prev   = prev,
       step   = i,
       simple = ifelse(i==1, TRUE, FALSE),
       remote = FALSE,
       removefolder=FALSE, 
       clean=1,
       run.on.sge=F)

# Extract parameter estimates -- Could be done with xpose for more genera solution 
params <- readEstPar(reserr  = "comb",
                     mpar    = missp[[1]],
                     simple  = ifelse(i==1, TRUE, FALSE),
                     numeric = TRUE)
prev[[i]]$params <- params

## get params and RSE from xpose
library(xpose4)if
pars <- getPars("runfE.mod.dir.3/NM_run1/psn.lst")
print(pars,col.names=TRUE)

## should be the end of the loop
i=2
i=3
i=4

# Create function_input file     
createFunctionInput(prev     = prev,                # previous run info
                    cur.cov  = list(init=c(1), min=c(1),  max=c(70)), # current init/min/max cov values
                    cur.groupsize = list(init=nid, min=nid,  max=nid), # current init/min/max group size
                    unfix    = list(theta = "1 1 1 1 1", omega="1 1", sigma="1 1", parDsUnInteresting = "0 0 0 0 0 0 0 0 0"), # fixed / Ds parameters
                    samples  = list(init=list(c(0.5,1,2,3,6,12,24))),             # current times + number
                    settings = list(remote = FALSE,
                                    optimizedSamplingtimes=TRUE, 
                                    optimizedCovariates=TRUE,
                                    ODtype="D", ODcalctype="detFIM", # xD or xDs
                                    runRemote=FALSE),           # optimization settings
                    simple   = ifelse(i==2, TRUE, FALSE),   # 2nd step use prespecified pars
                    mpar     = missp[[1]])                 # prespecified parameters

# Run PopED
runPoped(remote = FALSE, name=name, sh.script="run_andy.sh",cluster=F)

# get poped output file
file.name <- getPopedOutputFile()

# Extract OFV
ofv <- getPopedOfv()

# Extract Optimal covariate values
optcov <- getOptimalCov(file.name)

# Save current run results to previous
current <- list(nsteps        = nsteps,
                ngroups       = 1, 
                prev          = prev,
                dose          = list(1000),     # necessery for makeDataSim
                nid           = nid, # necessery for makeDataSime
                models        = models,
                cur.cov       = list(init=c(1), min=c(1),  max=c(70)),
                unfix         = list(theta = "1 1 1 1 1", omega="1 1", sigma="1 1", parDsUnInteresting = "0 0 0 0 0 0 0 0 0"), # fixed / Ds parameters
                cur.groupsize = list(init=nid, min=nid,  max=nid),
                samples       = list(init=list(c(0.5,1,2,3,6,12,24))),
                settings      = list(remote = FALSE,
                                     optimizedSamplingtimes=TRUE, 
                                     optimizedCovariates=TRUE,
                                     ODtype="D", ODcalctype="detFIM", # xD or xDs
                                     runRemote=FALSE),
                mpar          = missp[[1]],
                name          = name,
                dataset       = read.table("outA.tab"),
                cov           = optcov$optCov,
                samplingtimes = optcov$optTimes,
                ofv           = ofv,
                params        = params)
  
  prev[[i]] <- current
  
  
  # SSE for last run
  save(x=prev, file='prev.Rdata')
  if(i == nsteps){
    makeSimFile(prev = prev, step = i+1, last=TRUE, fixed=fixed)
    runSSE(prev      = prev,
           step      = i+1,
           simple    = ifelse(i==1, TRUE, FALSE),
           remote    = settings$runRemote,
           name      = name)
    params <- readEstPar(reserr  = "comb",
                         mpar    = mpar,
                         simple  = ifelse(i==1, TRUE, FALSE),
                         numeric = TRUE)
    prev[[i]]$params <- params       
  }
  
  
  cleanup()
  
}
return(prev) 
}


####################

res$description<-description
save(x=res, file=paste("../resultI_", name, ".Rdata", sep=""))
setwd("..")

##################
# old stuff
###################
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
              models = list(modfullest="run2fE.mod", modfullsim="run2fS.mod", modredest="run2rE.mod", modredsim="run2rS.mod"),
              cur.cov=list(init=c(1), min=c(1),  max=c(70)),
              unfix=list(theta = "1 1 1 1 1", omega="0 0", sigma="0 0", parDsUnInteresting = "1 1 0 0 0 1 1 1 1"),
              cur.groupsize=list(init=nid, min=nid,  max=nid),
              samples=list(init=list(c(0.5,1,2,3,6,12,24))),
              settings=list(remote = FALSE,
                optimizedSamplingtimes=TRUE, optimizedCovariates=TRUE,
                ODtype="D", ODcalctype="Ds", # xD or xDs
                runRemote=FALSE),
              mpar=m, fixed=FALSE,
              description=description
              )
    summary<-summaryResults(name=name)   
    evalDesign(name=name, summary=summary, description=description,
               samples=samples,
               ngroups=nsteps, nsteps=nsteps, misspec=m)
    count=count+1
  }
}
