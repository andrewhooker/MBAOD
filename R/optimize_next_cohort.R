optimize_next_cohort <- function (prev, cur.cov, cur.groupsize, unfix, 
                                  samples, settings, i, mpar, out.lst, est.mods, name,
                                  tool="poped_R") {
  
  if(tool=="poped_matlab"){
    ## Create function_input file  
    createFunctionInput(prev     = prev,                # previous run info
                        cur.cov  = cur.cov,             # current init/min/max cov values
                        cur.groupsize = cur.groupsize, # current init/min/max group size
                        unfix    = unfix,                 # fixed / Ds parameters
                        samples  = samples,             # current times + number
                        settings = settings,           # optimization settings
                        simple   = ifelse(i==2, TRUE, FALSE),   # 2st step use prespecified pars
                        mpar     = mpar,  # prespecified parameters
                        out.lst=out.lst,
                        est.mods=est.mods)                 
    
    ## Run PopED
    runPoped_matlab(remote = settings$runRemote, name=name, sh.script=settings$poped.sh.script, cluster=settings$poped.cluster)
    
    ## get poped output file
    file.name <- getPopedOutputFile()
    
    ## Extract OFV
    ofv <- getPopedOfv()
    
    ## Extract Optimal covariate values
    optvars <- getOptimalCov(file.name)
  }
  if(tool=="poped_R"){
    ## Create poped.db  
    poped.db <- createPopedDB(prev     = prev,                # previous run info
                              cur.cov  = cur.cov,             # current init/min/max cov values
                              cur.groupsize = cur.groupsize, # current init/min/max group size
                              unfix    = unfix,                 # fixed / Ds parameters
                              samples  = samples,             # current times + number
                              settings = settings,           # optimization settings
                              simple   = ifelse(i==2, TRUE, FALSE),   # 2st step use prespecified pars
                              mpar     = mpar,  # prespecified parameters
                              out.lst=out.lst,
                              est.mods=est.mods)   
    
    cat("-- Optimizing cohort ",i,"\n")
    
    #     output <- poped_optimize(poped.db,
    #                              #opt_xt=1,opt_a=1,
    #                              bUseRandomSearch= 1,rsit=300,
    #                              bUseStochasticGradient = 0,bUseBFGSMinimizer = 0,bUseLineSearch = 0)
    
    #     dsl <- downsizing_general_design(poped.db)
    #     output <- Doptim(poped.db,dsl$ni, dsl$xt, dsl$model_switch, dsl$x, dsl$a, 
    #                      dsl$bpop, dsl$d, dsl$maxxt, dsl$minxt,dsl$maxa,dsl$mina,
    #                      bUseRandomSearch= 1,
    #                      bUseStochasticGradient = 0,bUseBFGSMinimizer = 0,bUseLineSearch = 0,
    #                      trflag=F) 
    
    
    output <- RS_opt(poped.db,trflag=settings$trflag)
    ofv <- output$dmf
    
    optCov.tmp <- output$poped.db$ga
    optCov <- list()
    for(ii in 1:dim(optCov.tmp)[[1]]){
      optCov[[ii]] <- optCov.tmp[ii,]
    }
    if(!settings$trflag){
      tmp_txt <- "\nOptimized Covariates"
      tmp_txt <- paste(tmp_txt,':\n',sep="")
      #fprintf(fn,tmp_txt)
      fprintf(tmp_txt)
      for(ct1 in 1:poped.db$m){
        fprintf('Group %g: ', ct1)
        for(ct2 in 1:poped.db$na){
          tmp_txt <- '%g'
          if(ct2<poped.db$na) tmp_txt <- paste(tmp_txt,' : ',sep="")
          fprintf(tmp_txt,output$poped.db$ga[ct1,ct2])
        }
        fprintf('\n')
      }
      fprintf('\n')
    }
    
    optTimes.tmp <- output$poped.db$gxt
    optTimes <- list()
    for(ii in 1:dim(optTimes.tmp)[[1]]){
      optTimes[[ii]] <- optTimes.tmp[ii,]
    }
    if(!settings$trflag){
      print_xt(output$poped.db$gxt,output$poped.db$gni,output$poped.db$global_model_switch,head_txt="Optimized Sampling Schedule\n")
      fprintf('\n')
    }
    
    optvars=list(optCov=optCov,optTimes=optTimes) 
  }
  
  return(list(optvars=optvars,ofv=ofv))
}