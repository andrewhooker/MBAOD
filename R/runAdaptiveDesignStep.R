runAdaptiveDesignStep <- function(nsteps,
                                  prev,
                                  models,
                                  cur.cov,
                                  unfix,
                                  cur.groupsize,
                                  samples,
                                  settings, # detFIM or Ds
                                  mpar,fixed,
                                  # initial mat. parameters
                                  name,
                                  OD_tool="poped_R"){
  
  # Cleanup   
  cleanup()
  
  # Copy model files to default names               
  #copyModels(models)
  
  # output file names from NONMEM
  est.mods <- c(reduced=models$modredest,full=models$modfullest)
  out.lst <- gsub("(.*)(\\.[^\\.]*)$","\\1",est.mods)
  out.lst <- paste(out.lst,".lst",sep="")
  names(out.lst) <- names(est.mods)
  
  # Run adaptive thing
  for(i in 1:nsteps){
    print('')
    print('----------------------')
    print(paste('--------- Running Step', i))
    print('----------------------')
    print('')
    if(i!=1){
      
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
    ## Generate input dataset for SSE
    if(i==2 && fixed){
      makeSimFile_nonmem(prev = prev, step=i, last=TRUE,fixed=TRUE)
    } else {
      makeSimFile_nonmem(prev = prev, step=i, last=FALSE)
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
  return(prev) 
}
