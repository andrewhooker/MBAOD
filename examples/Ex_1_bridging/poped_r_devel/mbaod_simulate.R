mbaod_simulate <- function(cohorts,                
                           ncohorts,
                           rep,      
                           AOD_rules=list(keep_data=TRUE,update_parameters=TRUE),
                           name = "MBAOD_run",
                           description = NULL,
                           settings=list(
                             remote = FALSE,
                             runRemote=FALSE,
                             poped.cluster=FALSE,
                             sse.remove.folder=TRUE, 
                             sse.clean=3,
                             sse.run.on.sge=FALSE,
                             trflag=FALSE,
                             fixed=FALSE
                           ),
                           ...){
  # check if directory exists
  valid_name  <- FALSE
  i <- 0
  while(!valid_name){
    i <- i+1
    run_dir <- paste(name,"_run_dir_",i,sep="")
    valid_name <- !file.exists(run_dir)
  }
  dir.create(run_dir)
  
  
  # create the extra cohorts if needed
  if(length(cohorts)<ncohorts) cohorts[(length(cohorts)+1):ncohorts] <- cohorts[length(cohorts)]
  
  ## copy needed files to directory
  #file.copy(c(unlist(models)), name, overwrite=TRUE) # copy model files
  #if(!is.null(design.files)) file.copy(c(unlist(design.files)), name, overwrite=TRUE) # copy design files
  #file.copy(c(unlist(settings$poped.sh.script)), name, overwrite=TRUE) # copy shell scripts
  
  #setwd(paste("./",name,sep=""))  
  
  #models_names <- lapply(models,basename)
  #design_file_names <- lapply(design.files,basename) 
  #settings$poped.sh.script <- basename(settings$poped.sh.script)
  
  results_all<-list()
  for(j in 1:rep){
    
    print('')
    print('----------------------')
    print(paste('--------- Running Iteration', i))
    print('----------------------')
    print('')
    
    rep_dir <- file.path(run_dir,paste("rep_",j,sep=""))
    dir.create(rep_dir)
    
    
    # Cleanup   
    #cleanup()
    
    # Copy model files to default names               
    #copyModels(models)
    
    # output file names from NONMEM
    #     est.mods <- c(reduced=models$modredest,full=models$modfullest)
    #     out.lst <- gsub("(.*)(\\.[^\\.]*)$","\\1",est.mods)
    #     out.lst <- paste(out.lst,".lst",sep="")
    #     names(out.lst) <- names(est.mods)
    
    # Simulate one MBAOD experiment
    for(i in 1:ncohorts){
      print('')
      print('----------------------')
      print(paste('--------- Running Step', i))
      print('----------------------')
      print('')
      
      cohort_dir <- file.path(rep_dir,paste("cohort_",i,sep=""))
      dir.create(cohort_dir)
      
      cohort <- cohorts[[i]]
      
      
      
      #----------  optimize the cohort --------
      if(!is.null(cohort$optimize)){
        
        ret <- optimize_next_cohort(prev, cur.cov, cur.groupsize, unfix, 
                                    samples, settings, i, mpar, out.lst, est.mods, name,tool=OD_tool)
        ofv <- ret$ofv
        optvars <- ret$optvars
        ## Save current run results to previous
        current <- list(nsteps        = nsteps,
                        ngroups       = length(cur.groupsize$init), 
                        prev          = prev,
                        dose          = list(1000),     # necessery for makeDataSim
                        nid           = list(cur.groupsize$init), # necessery for makeDataSime
                        models        = models,
                        cur.cov       = cur.cov,
                        unfix         = unfix,
                        cur.groupsize = cur.groupsize,
                        samples       = samples,
                        settings      = settings,
                        mpar          = mpar,
                        name          = name,
                        dataset       = read.table("outA.tab"),
                        cov           = optvars$optCov,
                        samplingtimes = optvars$optTimes,
                        ofv           = ofv,
                        params        = params)
        
        prev[[i]] <- current
      }
      
      #----- simulate  the  cohort ---------
      if(!is.null(cohort$simulate)){
        if(cohort$simulate$target=="NONMEM"){
          browser()
          ## create data set
          sim_data <- model_prediction(DV=T,
                                       design=cohort$design,
                                       dosing=cohort$simulate$dosing,
                                       filename=file.path(cohort_dir,"sim_data.csv"))
          
          ## need to add ability to attach old data from previous cohorts and priors
          #       ## Generate input dataset for SSE
          #       if(i==2 && fixed){
          #         makeSimFile_nonmem(prev = prev, step=i, last=TRUE,fixed=TRUE)
          #       } else {
          #         makeSimFile_nonmem(prev = prev, step=i, last=FALSE)
          #       }
      
          ## copy simulation model to directory with name sim.mod
          ## change $DATA to right file name (sim.data.csv)
          ## change $INPUT so that it matches sim_data, match with grep, if no match then throw an error, add drop to columns not needed
          ## add table output so that you get same as $INPUT
          
          
          
          ## copy estimation model to directory with name est.mod
          ## change $DATA to match sim data name with simulated data "sim_data_2"
          ## change $INPUT so that it matches sim_data_2, match with grep, if no match then throw an error, add drop to columns not needed
          
          
          
        }
      }
      
      # Generate simulation and re-estimation (est: including previous runs)
      runSSE(prev   = prev,
             step   = i,
             simple = ifelse(i==1, TRUE, FALSE),
             remote = settings$runRemote,
             name   = name,
             removefolder=settings$sse.remove.folder, 
             clean=settings$sse.clean,
             run.on.sge=settings$sse.run.on.sge,
             models=models)
      
      
      # Extract parameter estimates    
      params <- readEstPar(reserr  = "comb",
                           mpar    = mpar,
                           simple  = ifelse(i==1, TRUE, FALSE),
                           numeric = TRUE,
                           out.lst)
      prev[[i]]$params <- params
      
      ## get params and RSE from xpose
      ##library(xpose4)
      ##pars <- getPars("runfE.mod.dir.3/NM_run1/psn.lst")
      ##print(pars,col.names=TRUE)
      
      # or ...
      #   output_filename <- out.lst["full"]
      #   if(i==1) output_filename <- out.lst["reduced"]
      #   prev[[i]]$params <- xpose4::read.lst(output_filename)    
      #   
      
      cleanup(sse.remove.folder=settings$sse.remove.folder)
      
    }
    res <- prev
    res$description<-description
    resall[[i]]<-res
    
  }
  
  save(x=resall, file=paste("../resultI_", name, ".Rdata", sep=""))
  
  setwd("..")
  
}
