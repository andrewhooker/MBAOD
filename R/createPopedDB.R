# function to update the function input file
createPopedDB<- function(prev,      # previous run info
                         cur.cov,          # current init/min/max cov values. i.e. list=c(init=c(40,60), ...
                         cur.groupsize,    # current init/min/max group size i.e. list=c(init=c(10,10), ...=
                         unfix,            # fixed / Ds parameters                              
                         samples,          # current times + number          i.e. samples$times=list(c(1,2,3), c(1,2,3), ..)
                         settings,          # OD settings
                         simple,
                         mpar,
                         out.lst,
                         est.mods,
                         ...){ # things passed to create.poped.database
  
  
  
  
  
  ### Optimization settings
  # number of groups
  cur.ngroups  <- length(samples$init)
  prev.ngroups <- unlist(lapply(prev, function(x) return(x$ngroups)))
  popedInput.m <- sum(cur.ngroups, prev.ngroups)
  
  # optimize for sampling times and/or covariates
  #  popedInput.optsw <- ifelse(settings$optimizedSamplingtimes, "0 1 0 1 0", "0 0 0 1 0")
  popedInput.optsw <- cbind(0,0,0,0,0)
  if(settings$optimizedSamplingtimes) popedInput.optsw[2] <- 1
  if(settings$optimizedCovariates) popedInput.optsw[4] <- 1
  
  ### Parameter estimates
  estPar<- readEstPar(reserr="comb", mpar=mpar, simple=simple,out.lst=out.lst) # Extract parameter estimates from LST
  #  print(mpar)
  #  print(simple)
  print(estPar)  
  
  tmp <- gsub(";","",estPar$estTh)
  tmp.2 <- gsub("(\\d+)(\\s+)","\\1,\\2",tmp)
  tmp.3 <- as.numeric(strsplit(tmp.2,"\\s*,\\s*")[[1]])
  bpop <- matrix(tmp.3,ncol=3,byrow=T)
  
  tmp <- gsub(";","",estPar$estOm)
  tmp.2 <- gsub("(\\d+)(\\s+)","\\1,\\2",tmp)
  tmp.3 <- as.numeric(strsplit(tmp.2,"\\s*,\\s*")[[1]])
  d <- matrix(tmp.3,ncol=3,byrow=T)
  
  num.sig <- length(gregexpr(";",estPar$estSig))+1
  tmp <- gsub(";","",estPar$estSig)
  tmp.2 <- gsub("(\\d+)(\\s+)","\\1,\\2",tmp)
  tmp.3 <- as.numeric(strsplit(tmp.2,"\\s*,\\s*")[[1]])
  sigma <- matrix(tmp.3,ncol=num.sig,byrow=T)
  
  ### Fixed and un-interesting parameters / optimal design type
  d_switch <- ifelse(settings$ODtype == "ED", 0, 1);
  ofv_calc_type <- ifelse(settings$ODcalctype == "detFIM", 1, 6)
  
  ### covariates
  #  prev.cov     <- unlist(lapply(prev, function(x) return(x$cov)))
  prev.cov     <- unlist(lapply(prev[[length(prev)]]$cov, function(x) return(x)))
  #   design.a     <- paste(c(prev.cov, cur.cov$init), collapse=" ")
  #   design.maxa  <- paste(c(prev.cov, cur.cov$max), collapse=" ")
  #   design.mina  <- paste(c(prev.cov, cur.cov$min), collapse=" ")  
  design.a     <- cbind(c(prev.cov, cur.cov$init))
  design.maxa  <- cbind(c(prev.cov, cur.cov$max))
  design.mina  <- cbind(c(prev.cov, cur.cov$min))
  
  ### Group sizes
  prev.groupsize         <- unlist(lapply(prev, function(x) return(x$cur.groupsize$init)))
  ##prev.groupsize         <- prev$groupsize
  #design.groupsize       <- paste(c(prev.groupsize, cur.groupsize$init), collapse=" ")
  design.groupsize       <- cbind(c(prev.groupsize, cur.groupsize$init))
  #design.maxgroupsize    <- paste(c(prev.groupsize, cur.groupsize$max), collapse=" ")
  #design.mingroupsize    <- paste(c(prev.groupsize, cur.groupsize$min), collapse=" ")
  #design.maxtotgroupsize <- sum(prev.groupsize, cur.groupsize$max)
  #design.mintotgroupsize <- sum(prev.groupsize, cur.groupsize$min)
  
  ### Sampling times/numbers
  # number of samples for each group
  cur.nrsamples  <- unlist(lapply(samples$init, function(x) length(x))         )
  #  prev.nrsamples <- unlist(lapply(prev, function(x) length(unlist(x$samplingtimes))) )
  prev.nrsamples <- unlist(lapply(prev[[length(prev)]]$samplingtimes, function(x) length(unlist(x))))
  
  tot.nrsamples  <- paste(c(prev.nrsamples, cur.nrsamples),  collapse=" ; ") # CHARACTER
  tot.nrsamplesN <- as.numeric(strsplit(tot.nrsamples, "; ")[[1]]) #NUMERIC
  
  
  # sampling times for each group (including previous)
  #cur.times      <- paste(unlist(lapply(samples$init, function(x) c(";",x)))[-1] , collapse=" ")
  cur.times <- matrix(unlist(samples$init),nrow=length(samples$init))
  #prev.times     <- unlist(lapply(prev[[length(prev)]]$samplingtimes, function(x)  c(";" , x)))[-1]
  prev.times <- matrix(unlist(prev[[length(prev)]]$samplingtimes),nrow=length(prev[[length(prev)]]$samplingtimes))
  #prev.times     <- gsub("1e-05", "0", prev.times) # poped uses 1e0-5 for zero.    
  #design.xt      <- paste(prev.times, ";", cur.times, collapse=" ")
  design.xt <- rbind(prev.times,cur.times)
  
  # min/max times calculation, for each group
  cur.maxxt <- matrix(24,dim(cur.times)[[1]],dim(cur.times)[[2]])
  cur.minxt <-  matrix(0,dim(cur.times)[[1]],dim(cur.times)[[2]])
  prev.maxxt <- prev.times
  prev.minxt <- prev.times
  design.maxxt <- rbind(prev.maxxt,cur.maxxt)
  design.minxt <- rbind(prev.minxt,cur.minxt) 
  

  
  unfix_num <- unfix
  for(i in names(unfix)){
    tmp <- gsub("(\\d+)(\\s+)","\\1,\\2",unfix[[i]])
    tmp.2 <- as.numeric(strsplit(tmp,"\\s*,\\s*")[[1]])
    unfix_num[[i]] <- tmp.2
  }
    
  # initial design  
  poped.db <- create.poped.database(ff_file=settings$ff_file,
                                    fError_file=settings$fError_file,
                                    fg_file=settings$fg_file,
                                    sigma=sigma,
                                    bpop=bpop, 
                                    d=d, 
                                    notfixed_d=unfix_num$omega,
                                    notfixed_sigma=unfix_num$sigma,
                                    notfixed_bpop=unfix_num$theta,
                                    a=design.a,
                                    maxa=design.maxa,
                                    mina=design.mina,
                                    m=popedInput.m,
                                    groupsize=design.groupsize,
                                    #mingroupsize=design.mingroupsize,
                                    #maxgroupsize=design.maxgroupsize,
                                    #mintotgroupsize=design.mintotgroupsize,
                                    #maxtotgroupsize=design.maxtotgroupsize,
                                    optsw=popedInput.optsw,
                                    ofv_calc_type=ofv_calc_type,
                                    d_switch=d_switch,
                                    ds_index=unfix$parDsUnInteresting,
                                    #ni=tot.nrsamples,
                                    #maxni=max(tot.nrsamplesN),
                                    #minni=max(tot.nrsamplesN),
                                    #model_switch=mswitch,
                                    xt=design.xt,
                                    minxt=design.minxt,
                                    maxxt=design.maxxt,
                                    ...)
  
  
  return(poped.db)
}