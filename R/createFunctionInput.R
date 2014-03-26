# function to update the function input file
createFunctionInput <- function(prev,      # previous run info
                                cur.cov,          # current init/min/max cov values. i.e. list=c(init=c(40,60), ...
                                cur.groupsize,    # current init/min/max group size i.e. list=c(init=c(10,10), ...=
                                unfix,            # fixed / Ds parameters                              
                                samples,          # current times + number          i.e. samples$times=list(c(1,2,3), c(1,2,3), ..)
                                settings,          # OD settings
                                simple,
                                mpar,
                                out.lst,
                                est.mods
){
  
  wline <- function(var, val, transpose=FALSE, comment=""){
    # Function to write a line in the function_input
    t="" # no transpose sign
    if(transpose == TRUE){t="'"} # transpose at end of line
    line <- paste(var,"=[", val,"]", t, ";  %", comment, sep="")  # line poped
    return(line)
  }
  
  ### Create empty matrix
  f <- matrix(nrow=60, ncol=1)
  
  
  #prev<-prev[[length(prev)]]
  
  ### Optimization settings
  # number of groups
  cur.ngroups  <- length(samples$init)
  prev.ngroups <- unlist(lapply(prev, function(x) return(x$ngroups)))
  
  popedInput.m <- sum(cur.ngroups, prev.ngroups)
  
  # optimize for sampling times and/or covariates
  #  popedInput.optsw <- ifelse(settings$optimizedSamplingtimes, "0 1 0 1 0", "0 0 0 1 0")
  if(settings$optimizedSamplingtimes & settings$optimizedCovariates){ popedInput.optsw<- "0 1 0 1 0"}
  if(!settings$optimizedSamplingtimes & settings$optimizedCovariates){ popedInput.optsw<-  "0 0 0 1 0"}
  if(settings$optimizedSamplingtimes & !settings$optimizedCovariates){ popedInput.optsw<- "0 1 0 0 0"} 
  
  f[1]  <- "function [popedInput] = function_input()"
  f[2]  <- wline("popedInput.m"                    ,popedInput.m      , FALSE, "Number of groups")  
  f[3]  <- wline("popedInput.iApproximationMethod" ,0                 , FALSE, "FO(0) FOCE(1) FOCEI(2) FOI(3)")
  f[4]  <- wline("popedInput.iEDCalculationType"   ,0                 , FALSE, "ED integr calcul (0=MC, 1=LAP, 2=BFGS)")
  f[5]  <- wline("popedInput.prior_fim"            ,"zeros(0,1)"      , FALSE, "Use Prior FIM")
  f[6]  <- wline("popedInput.optsw"                , popedInput.optsw , FALSE, "Smpl/subject, Smpl sched, Discrete, Covariates, Num ind/grp" )
  
  ### Parameter estimates
  estPar<- readEstPar(reserr="comb", mpar=mpar, simple=simple,out.lst=out.lst) # Extract parameter estimates from LST
  #  print(mpar)
    print(simple)
    print(estPar)
  f[7]  <- ""
  f[8]  <- "%----- Parameter estimates ----"
  f[9]  <- wline("popedInput.design.bpop"      , estPar$estTh  , FALSE , "Typical estimates")  
  f[10] <- wline("popedInput.design.d"         , estPar$estOm  , FALSE , "BSV estimates")
  f[11] <- wline("popedInput.design.covd"      , 0             , FALSE , "Covar estimates BSV")
  f[12] <- wline("popedInput.design.docc"      , "zeros(3,0)"  , TRUE  , "BOV estimates")
  f[13] <- wline("popedInput.design.covdocc"   , "zeros(0,1)"  , TRUE  , "Covar estimates BOV")
  f[14] <- wline("popedInput.design.sigma"     , estPar$estSig , FALSE , "RUV estimates Variance-covariance") 
  
  ### Fixed and un-interesting parameters / optimal design type
  d_switch <- ifelse(settings$ODtype == "ED", 0, 1);
  ofv_calc_type <- ifelse(settings$ODcalctype == "detFIM", 1, 6)
  f[15] <- ""
  f[16] <- "%----- Un intesting parameters -----"
  f[17] <- wline("popedInput.d_switch"         , d_switch                   , FALSE, "Use D (1) or ED (2) optimal design")  
  f[18] <- wline("popedInput.ofv_calc_type"    , ofv_calc_type              , FALSE, "1=det(FIM), 4=log(det(FIM)), 6=Ds")
  f[19] <- wline("popedInput.CriterionOptions.ds_index", unfix$parDsUnInteresting , FALSE, "Unintersting(=1) parameters")   
  f[20] <- wline("popedInput.notfixed_bpop"    , unfix$theta                 , FALSE, "Unfixed(1) Thetas") 
  f[21] <- wline("popedInput.notfixed_d"       , unfix$omega                 , FALSE, "Unfixed(1) Omegas")
  f[22] <- wline("popedInput.notfixed_sigma"   , unfix$sigma                 , FALSE, "Unfixed(1) Sigmas")
  f[23] <- wline("popedInput.notfixed_covd"    , 0                          , FALSE, "Unfixed (1) Covariance BSV" )
  f[24] <- wline("popedInput.notfixed_docc"    , "zeros(0,1)"               , TRUE , "Unfixed(1) BOV")
  f[25] <- wline("popedInput.notfixed_covdocc" , "zeros(0,1)"               , TRUE , "Unfixed(1) Covariance BOV")
  f[26] <- wline("popedInput.notfixed_covsigma", "zeros(0,1)"               , TRUE , "Unfixed(1) Covariance Sigma")
  
  ### Number of parameters
  numPar <- readNumPar(est.mods["full"]) # Read number of parameters
  f[27]  <- ""
  f[28]  <- "% ---- Numbers of parameters -----"
  f[29]  <- wline("popedInput.ng"     , numPar$nth+1  , FALSE , "Number of structural parameters + covariates + discrete covariates")
  f[30]  <- wline("popedInput.nbpop"  , numPar$nth    , FALSE , "Number of population parameters")
  f[31]  <- wline("popedInput.nb"     , numPar$nom    , FALSE , "Number of BSV parameters")
  
  ### Covariates
  # cov<-list()
  # cov$init <- unlist(lapply(cur.cov$init, function(x) {x}))
  # cov$max  <- unlist(lapply(cur.cov$max, function(x) {x}))
  # cov$min  <- unlist(lapply(cur.cov$min, function(x) {x}))
  
  
  #  prev.cov     <- unlist(lapply(prev, function(x) return(x$cov)))
  prev.cov     <- unlist(lapply(prev[[length(prev)]]$cov, function(x) return(x)))
  
  
  design.a     <- paste(c(prev.cov, cur.cov$init), collapse=" ")
  design.maxa  <- paste(c(prev.cov, cur.cov$max), collapse=" ")
  design.mina  <- paste(c(prev.cov, cur.cov$min), collapse=" ")  
  
  design.Ga    <- paste(seq(1, popedInput.m, 1), collapse=" ")
  line_opta    <- paste(rep(1, popedInput.m), collapse=" ")
  
  f[32] <- ""
  f[33] <- "% ---- Covariates ----- "
  f[34] <- wline("popedInput.na"           , 1           , FALSE , "Number of different covariates")
  f[35] <- wline("popedInput.design.a"     , design.a    , TRUE , "Initial covariate value per group")
  f[36] <- wline("popedInput.design.maxa"  , design.maxa , TRUE , "Maximum covariate value per group")
  f[37] <- wline("popedInput.design.mina"  , design.mina , TRUE , "Minimum covariate value per group")
  f[38] <- wline("popedInput.bUseGrouped_a", 0           , FALSE , "Use grouped covariate values (no=0)")
  f[39] <- wline("popedInput.design.Ga"    , design.Ga   , TRUE , "Grouping of covariate values")
  f[40] <- wline("popedInput.line_opta"    , line_opta   , FALSE , "Line search for each of the covariates") 
  
  ### Group sizes
  
  prev.groupsize         <- unlist(lapply(prev, function(x) return(x$cur.groupsize$init)))
  ##prev.groupsize         <- prev$groupsize
  
  design.groupsize       <- paste(c(prev.groupsize, cur.groupsize$init), collapse=" ")
  design.maxgroupsize    <- paste(c(prev.groupsize, cur.groupsize$max), collapse=" ")
  design.mingroupsize    <- paste(c(prev.groupsize, cur.groupsize$min), collapse=" ")
  design.maxtotgroupsize <- sum(prev.groupsize, cur.groupsize$max)
  design.mintotgroupsize <- sum(prev.groupsize, cur.groupsize$min)
  
  f[41] <- ""
  f[42] <- '% --- Group sizes ----'
  f[43] <- wline("popedInput.design.groupsize"       , design.groupsize       , TRUE  , "Number of subject per group")
  f[44] <- wline("popedInput.design.maxgroupsize"    , design.maxgroupsize    , TRUE  , "Maximum of subject per group")
  f[45] <- wline("popedInput.design.mingroupsize"    , design.mingroupsize    , TRUE  , "Minimum of subject per group")
  f[46] <- wline("popedInput.design.maxtotgroupsize" , design.maxtotgroupsize , FALSE , "Maximum total group size")
  f[47] <- wline("popedInput.design.mintotgroupsize" , design.mintotgroupsize , FALSE , "Minimum total group size")
  
  
  ### Sampling times/numbers
  # number of samples for each group
  cur.nrsamples  <- unlist(lapply(samples$init, function(x) length(x))         )
  #  prev.nrsamples <- unlist(lapply(prev, function(x) length(unlist(x$samplingtimes))) )
  prev.nrsamples <- unlist(lapply(prev[[length(prev)]]$samplingtimes, function(x) length(unlist(x))))
  
  tot.nrsamples  <- paste(c(prev.nrsamples, cur.nrsamples),  collapse=" ; ") # CHARACTER
  tot.nrsamplesN <- as.numeric(strsplit(tot.nrsamples, "; ")[[1]]) #NUMERIC
  
  # model switch, for each group
  mswitch        <- paste(unlist(lapply(tot.nrsamplesN, function(x) c(";", rep(1, x))))[-1], collapse=" ")
  
  # sampling times for each group (including previous)
  cur.times      <- paste(unlist(lapply(samples$init, function(x) c(";",x)))[-1] , collapse=" ")
  prev.times     <- unlist(lapply(prev[[length(prev)]]$samplingtimes, function(x)  c(";" , x)))[-1]
  prev.times     <- paste(gsub("1e-05", "0", prev.times), collapse=" ") # poped uses 1e0-5 for zero.    
  design.xt      <- paste(prev.times, ";", cur.times, collapse=" ")
  
  # min/max times calculation, for each group
  cur.maxxt <- paste(unlist(lapply(cur.nrsamples, function(x) c(";", rep(24, x))))[-1], collapse=" ")
  cur.minxt <- paste(unlist(lapply(cur.nrsamples, function(x) c(";", rep(0, x))))[-1], collapse=" ")
  prev.maxxt <- prev.times
  prev.minxt <- prev.times
  design.maxxt <- paste(c(prev.maxxt,";", cur.maxxt), collapse=" ") # 
  design.minxt <- paste(c(prev.minxt,";", cur.minxt), collapse=" ") # 
  
  # grouping of sampling times (all separate)
  ct<<-0
  design.G<-paste(unlist(lapply(tot.nrsamplesN, function(x) {
    x<-(x*(ct)+1):(x*(ct+1));
    x<-c(";",x)
    ct<<-ct+1; return(x)    
  }))[-1], collapse=" ")
  
  f[48] <- ""
  f[49] <- "% --- Sampling times --- "
  f[50] <- wline("popedInput.maxni"              , max(tot.nrsamplesN) , FALSE , "Maximum number of samples per group")
  f[51] <- wline("popedInput.minni"              , max(tot.nrsamplesN), FALSE , "Minimum number of samples per group")
  f[52] <- wline("popedInput.design.ni"          , tot.nrsamples, FALSE , "Initial number of samples per group")
  f[53] <- wline("popedInput.design.model_switch", mswitch      , FALSE , "Model switch for samples per group")
  f[54] <- wline("popedInput.design.xt"          , design.xt    , FALSE , "Initial sampling times per group")
  f[55] <- wline("popedInput.design.maxxt"       , design.maxxt , FALSE , "Maximum sampling times per group")
  f[56] <- wline("popedInput.design.minxt"       , design.minxt , FALSE , "Minimum sampling times per group")
  f[57] <- wline("popedInput.bUseGrouped_xt"     , 0            , FALSE , "Use grouped sampling times")
  f[58] <- wline("popedInput.design.G"           , design.G     , FALSE , "Grouping of sampling times")
  
  zerosGroup<-paste("zeros(0,",popedInput.m,")", sep="")
  f[59] <- wline("popedInput.design.x" , zerosGroup, TRUE, "Matrix initial discrete covariates")
  f[60] <- wline("popedInput.design.Gx", zerosGroup, TRUE, "Matrix group of discrete covariates")
  
  # read fixed part of function input
  r <- file("function_input_reduced.m")
  fr <- as.matrix(readLines(r), ncol=1)
  close(r)
  
  # combined fixed and dynamic part 
  full <- rbind(f, fr)
  
  # write to file
  o <- file("function_input.m")
  writeLines(full, o)
  close(o)
  
}