copyModels <- function(models){
  # function to write a line in poped/matlab format  
  print('- Copying models')
  
  # copy files to current model files
  file.copy(models$modfullest, 'runfE.mod', overwrite=TRUE)
  file.copy(models$modfullsim, 'runfS.mod', overwrite=TRUE)
  file.copy(models$modredest , 'runrE.mod', overwrite=TRUE)
  file.copy(models$modredsim , 'runrS.mod', overwrite=TRUE)
}


compute.auc <- function(final.par.list){
  AUC <- c()
  for(j in 1:dim(final.par.list)[1]){
    for(i in 1:70){
      ##j=1
      ##i=1
      final.par <- final.par.list[j,]
      wt=i
      dose=1000#*(wt/70)
      trueAUC=dose/(1+ (2 * wt^5)/ (25^5 + wt^5))
      obsAUC=dose/(final.par$thCl+ (final.par$thMax * wt^final.par$thHill)/ (final.par$thE50^final.par$thHill + wt^final.par$thHill))
      AUC <- rbind(AUC,c(iter=j,wt=wt,dose=dose,trueAUC=trueAUC,obsAUC=obsAUC))
    }
  }
  AUC.2 <- as.data.frame(AUC)
  return(AUC.2)
}

extract.ree <- function(summary) {  
  #summary[[2]][1:20,1:10]
  
  #!duplicated(summary[[2]]$rep,fromLast=T)
  #foo<-summary[[2]][!duplicated(summary[[2]]$rep,fromLast=T),]
  #foo[1:20,1:10]
  
  #final.par <- subset(summary[[2]],grp==4)
  #browser()
  final.par <- summary[[2]][!duplicated(summary[[2]]$rep,fromLast=T),]
  cat("Number of parameter sets:",dim(final.par)[1])
  final.par.ree <- final.par
  final.par.ree$thCl <- (final.par$thCl - 1)/1 * 100
  final.par.ree$thV <- (final.par$thV - 20)/20 * 100
  final.par.ree$thMax <- (final.par$thMax - 2)/2 * 100
  final.par.ree$thE50 <- (final.par$thE50 - 25)/25 * 100
  final.par.ree$thHill <- (final.par$thHill - 5)/5 * 100  
  final.par.ree$omCL <- (final.par$omCL - 0.05)/0.05 * 100
  final.par.ree$omV <- (final.par$omV - 0.05)/0.05 * 100
  final.par.ree$sigP <- (final.par$sigP - 0.015)/0.015 * 100
  final.par.ree$sigA <- (final.par$sigA - 0.0015)/0.0015 * 100
  return(final.par.ree)
}



makeSimFile <- function(prev, step=1, last=FALSE, first=FALSE, fixed=FALSE){
  print('- Generating simulation file')

   # make empty data.framei
  dat <- data.frame(id  = numeric(0),
                    time= numeric(0),
                    dv  = numeric(0),
                    amt = numeric(0),
                    wt  = numeric(0))

  if(last==FALSE) {
    maxwt=length(prev[[step]]$cov)
    maxtimes=length(prev[[step]]$samplingtimes)

    dose  <- unlist(prev[[step]]$dose)  
    nid   <- unlist(prev[[step]]$nid)         
    wt    <- unlist(prev[[step]]$cov)[maxwt]
    times <- unlist(prev[[step]]$samplingtimes[[maxtimes]])

    times <- round(sort(times),3)
          
     # generate records per ID
    for(i in 1:nid){
      idose <- round(dose*(wt/70),3)
      obsi  <- data.frame(id=i, time=times, dv=NA, amt=NA, wt=wt)
      dosi  <- data.frame(id=i, time=0, dv=NA, amt=idose, wt=wt)
      idat  <- rbind(dosi, obsi)
      dat   <- rbind(dat, idat)
    } # end forloop ID
  }
  
  if(last==TRUE) {   
    maxngroup = length(unlist(prev[[step]]$cov))

    st=2 # dont use first 70kg group

    
    if(first==TRUE){st=1}

    if(fixed==TRUE){ # in case of fixed design
      prev[[step]]$nid<- list(c(unlist(prev[[1]]$nid),unlist(prev[[step]]$nid)))
    }
    
    for( g in st:maxngroup  ) {
        dose  <- unlist(prev[[step]]$dose)
        nid<- unlist(prev[[step]]$nid)
        if(length(nid) ==1 ){ nid   <- unlist(prev[[step]]$nid) }
        if(length(nid) > 1){ nid <- unlist(prev[[step]]$nid)[g] }
        
        wt    <- unlist(prev[[step]]$cov)[[g]]
        times <- unlist(prev[[step]]$samplingtimes[[g]])
      
        times <- round(sort(times),3)
      
        # generate records per ID
        for(i in 1:nid){
          idose <- round(dose*(wt/70),3)
          obsi  <- data.frame(id=i, time=times, dv=NA, amt=NA, wt=wt)
          dosi  <- data.frame(id=i, time=0, dv=NA, amt=idose, wt=wt)
          idat  <- rbind(dosi, obsi)
          dat   <- rbind(dat, idat)
        } # end forloop ID
        
      }
  }
 
  # write to file
  write.table(x=dat, "datarich.csv", row.names=FALSE, quote=FALSE, na=".") 
  return(dat) 
}



runPoped <- function(remote=remote, name=name, sh.script="run.sh",cluster=T){  
  print('- Running POPED')
  
  # remove old output.log
  unlink("output.log")
  
  # run optimization / call PopED
  #com="/usr/local/MATLAB/R2011b/bin/matlab -nosplash -nodisplay -nodesktop -r \"poped(function_input()); quit\" -logfile output.log"
  if(remote == TRUE) com <- paste("ssh andy@doris.biof.uu.se 'cd AOD/coen/",name,"; bash ",sh.script,"'", sep="")  
  if(remote == FALSE) com <- paste("bash ", sh.script, sep="")

  if(Sys.which("execute")=="") com <- paste("source ~/.bashrc;",com,sep="")
  
  print(com);
  
  # run poped
  if(!cluster){
      system(com)
    } else {
      system(com, wait=TRUE)
                                        # check if run is already finished
      run <- TRUE
      cat("Running")
      while(run == TRUE){
        
                                        #if(remote == TRUE) state <- system("ssh doris.biof.uu.se qstat", intern=TRUE)
                                        #if(remote == FALSE) state <- system("qstat", intern=TRUE)
        
                                        #if(length(state) > 0) { Sys.sleep(4)}
                                        #if(length(state) == 0) { print('finished poped'); run=FALSE}

                                        # read output file

        if(file.exists("output.log")){
          o<-file("output.log")
          f<-readLines(o)
          close(o)

          finished <- "              strengine: 'MATLAB 7.9.0.529 (R2009b)'"
          tail <- tail(f,2)[1]
          if( !is.na(tail) ) {
            if( is.logical(tail == finished)  ){
              if( tail == finished  ){
                print('Finished poped');
                run=FALSE;
              } else {
                cat('.'); Sys.sleep(3)
              }
            } else  {
              cat('.'); Sys.sleep(3)
            }
          } else {
            cat('.'); Sys.sleep(3)
          }    
        } 
      }
    }
}

getOptimalCov <- function(file.name="1.m"){
  
  print('- Extract optimal covariates')
  
  o <- file(file.name)
  out <- readLines(o, warn=FALSE)
  close(o)

  # covariates
  lineCov <- grep('popedOutput.a', out)
  optCov  <- out[lineCov]
  optCov  <- gsub("\\]",";",optCov)[[1]]
  optCov  <- gsub("\\[",";",optCov)[[1]]
  optCov  <- strsplit(optCov, c(";"))[[1]]
  optCov  <- optCov[!optCov %in% c("popedOutput.a=", "", "'", " ")]
  optCov  <- strsplit(optCov, ",")[[1]]
  cov=list()
  for(i in 1:length(optCov)){
    cov[[i]] <- as.numeric(optCov[i])
  }

  # sampling times
  lineTimes <- grep('popedOutput.xt', out)
  optTimes  <- paste(out[lineTimes:(lineCov-5)], collapse=" ")
  optTimes  <- gsub("\\]",";",optTimes)[[1]]
  optTimes  <- gsub("\\[",";",optTimes)[[1]]
  optTimes  <- strsplit(optTimes, ";")[[1]]
  optTimes  <- optTimes[!optTimes %in% c("popedOutput.xt=", "", " ")]
  times<-list()
  for(i in 1:length(optTimes)){
    timesi <- strsplit(optTimes[i], " ")[[1]]
    timesi <- timesi[!timesi %in% ""]
    times[[i]] <- as.numeric(timesi)
  }

  return(list(optTimes=times, optCov=cov))
}

getPopedOfv <- function(){  
  
  print('- Extract Poped final OFV/FIM')
  
  o <- file("output.log")
  out <- readLines(o)
  close(o)
  
  lineOFV <- grep("ofvmf:", out)
  ofv <- as.numeric(unlist(strsplit(out[lineOFV], ":"))[2])
  
  return(ofv)    
}


getPopedOutputFile <- function(){  
  
  print('- Extract Poped output file name')
  
  o <- file("output.log")
  out <- readLines(o)
  close(o)
  
  lineOUT <- grep("Result of optimization in file:", out)
  strsplit(out[lineOUT], ": ")
  file.name <- unlist(strsplit(out[lineOUT], ": "))[2]
  
  return(file.name)    
}






readUpdateFimCOI <- function(usepfim=TRUE, fixTh=c(1,1,1,1,1), fixCov=c(1),
                           fixOm=c(1,1), fixSig=c(1,1)){
  print('- Reading NONMEM fim file (if applicable)')  
  orderRow<-NULL # initialize variable for check  
  if(usepfim == TRUE){
    if(!file.exists('runfE.coi') & !file.exists('runrE.coi')){
      print('----> No Succesful Cov step, keep previous one')      
    } 
    if(file.exists('runfE.coi') | file.exists('runrE.coi')){      
      if(file.exists('runfE.coi')) fim<-read.table('runfE.coi', header=T, skip=1)
      if(file.exists('runrE.coi')) fim<-read.table('runrE.coi', header=T, skip=1)
      
      # determine line elements
      lineThe <- grep("THETA", fim[,1])
      lineOm  <- grep("OMEGA", fim[,1])
      lineSig <- grep("SIGMA", fim[,1])
      lineCov <- grep("(2,1)", fim[,1])             # covariances sigma, omega
      lineSig <- lineSig[!lineSig %in% lineCov[1]]
      lineOm  <- lineOm[!lineOm %in% lineCov[2]]
      
      # sort rows and column to PopED sequence      
      orderRow<-c(lineThe[which(fixTh==1)],lineOm[which(fixOm==1)],
                  lineCov[2][which(fixCov==1)],lineSig[which(fixSig==1)])
      orderRow<-orderRow[!is.na(orderRow)] # remove NA values      
      orderCol<-c(lineThe[which(fixTh==1)]+1,lineOm[which(fixOm==1)]+1,
                  lineCov[2][which(fixCov==1)]+1,lineSig[which(fixSig==1)]+1)
      orderCol<-orderCol[!is.na(orderCol)]
      
      if(length(orderRow)>0){
        fim<-fim[orderRow,]
        fim<-fim[,orderCol]
        
        # in case of simple model add additional columns for parameters not estimated.            
        if( length(grep("THETA", names(fim))) == 2 ){    
          fim <- cbind(fim[,1:2], matrix(0,ncol=3,nrow=nrow(fim)), fim[,3:ncol(fim)])
          row <- as.data.frame(matrix(0,ncol=ncol(fim),nrow=3))
          names(row) <- names(fim)
          fim <- rbind(fim[1:2,], row, fim[3:nrow(fim),])
        }      
      }
    }
  }
}




 



# evaluation function of design
# - extract mean optimal designs from final
# - plot change in OFV for each step
# - generate plots with bias/precision
# 

summaryResults<-function(name){

  load(paste("resultI_",name,".Rdata",sep=""))
  
  # empty result df's
  rawResultsCov <- data.frame();
  rawResultsPar <- data.frame();
  rawResultsSam <- data.frame()
  
  # Replications
  nrep <- length(resall)
  for(r in 1:nrep ){
    resRun <- resall[[r]]

    # Adaptive steps
    nstep <- length(resRun)
    nstep <- ifelse(nstep==1, nstep, nstep-1)
    for(s in (1:nstep) ){
      resRunStep <- resRun[[s]]

    # Number of groups in one step
      ngrp <- length(resRunStep$cov)
      nprev<-ifelse(s > 1, length(resRun[[s-1]]$cov), 0)
      for(g in (1+nprev):(ngrp) ){
        print(paste(r, s, g))

        dCov <- data.frame(name    = name,                           
                           nrep    = nrep,  rep     = r,
                           nsteps  = nstep, step    = s,
                           ngrp    = ngrp,  grp     = g,
                           nid     = unique(resRunStep$nid[[1]]),
                           ofv     = resRunStep$ofv,
                           cov     = resRunStep$cov[[ (g) ]],
                           description = resRun$description)

        dPar <- data.frame(name    = name,
                           nrep    = nrep,  rep     = r,
                           nsteps  = nstep, step    = s,
                           ngrp    = ngrp,  grp     = g,                 
                           ofv     = resRunStep$ofv,
                           thCl    = resRunStep$par$th[1],
                           thV     = resRunStep$par$th[2],
                           thMax   = resRunStep$par$th[3],
                           thE50   = resRunStep$par$th[4],
                           thHill  = resRunStep$par$th[5],
                           omCL    = resRunStep$par$om[1],
                           omV     = resRunStep$par$om[2],
                           sigP    = resRunStep$par$si[1],
                           sigA    = resRunStep$par$si[2],
                           description = resRun$description)

        # sort sampling times
        resRunStep$samplingtimes[[g]]<-sort( resRunStep$samplingtimes[[g]])
        
        dSam <- data.frame(name    = name,
                           nrep    = nrep,  rep     = r,
                           nsteps  = nstep, step    = s,
                           ngrp    = ngrp,  grp     = g,                 
                           ofv     = resRunStep$ofv,
                           s1      = resRunStep$samplingtimes[[g]][1],
                           s2      = resRunStep$samplingtimes[[g]][2],
                           s3      = resRunStep$samplingtimes[[g]][3],
                           s4      = resRunStep$samplingtimes[[g]][4],
                           s5      = resRunStep$samplingtimes[[g]][5],
                           s6      = resRunStep$samplingtimes[[g]][6],
                           s7      = resRunStep$samplingtimes[[g]][7],
                           description = resRun$description)

        # sort columns based on time
        dSamSort<-dSam[,order(dSam[,c("s1","s2","s3","s4","s5","s6", "s7")])+8]
        names(dSamSort)<-c("s1","s2","s3","s4","s5","s6", "s7")
        dSam<- cbind(dSam[,1:8], dSamSort)
   
        # save results to d.f.
        rawResultsCov <- rbind(rawResultsCov, dCov)
        rawResultsPar <- rbind(rawResultsPar, dPar)
        rawResultsSam <- rbind(rawResultsSam, dSam)      
               
      } # group
    } # step
  } # replications


  # number of individuals
  sumNid<-aggregate(rawResultsCov$nid, by=list(rawResultsCov$grp), median)
  
  # covariates
  sumCov<-aggregate(rawResultsCov$cov, by=list(rawResultsCov$grp), median)

  # sampling times
  # function(x){return(c(median(x),quantile(x, c(0.25,0.75))))}
  s1<-aggregate(rawResultsSam$s1, by=list(rawResultsSam$grp), median)
  s2<-aggregate(rawResultsSam$s2, by=list(rawResultsSam$grp), median)
  s3<-aggregate(rawResultsSam$s3, by=list(rawResultsSam$grp), median)
  s4<-aggregate(rawResultsSam$s4, by=list(rawResultsSam$grp), median)
  s5<-aggregate(rawResultsSam$s5, by=list(rawResultsSam$grp), median)
  s6<-aggregate(rawResultsSam$s6, by=list(rawResultsSam$grp), median)
  s7<-aggregate(rawResultsSam$s7, by=list(rawResultsSam$grp), median)
  sumTimes<-rbind(s1,s2,s3,s4,s5,s6,s7)
  sumTimes$sample<-sort(rep(rep(1:7),length(unique(sumTimes$Group.1))))
  
  return(list(rawResultsCov,
              rawResultsPar,
              rawResultsSam,
              sumTimes,
              sumCov))
} # function



evalDesign<-function(name, summary, description, prev=NULL, samples=5, misspec,
                     ngroups, nsteps){

 
  # generate input file
  if(is.null(prev)){
    prev<-list(list(dose = list(1000),
                    cov  = lapply( by(summary[[5]], summary[[5]]$Group.1, function(x) x),
                      function(x) x[2]),
                    nid  = list(summary[[1]]$nid),
                    samplingtimes = lapply(by(summary[[4]], summary[[4]]$Group.1, function(x) x$x),
                      function(x) x)))
  }

  makeSimFile(prev=prev, step=1, last=TRUE, first=TRUE, fixed=FALSE)
 
  # Perform simulation and re-estimation
  directory=paste("sse_",name,sep="")

    com1 <- paste("ssh andy@doris.biof.uu.se 'cd AOD/coen/;", sep="")
 #  com2 <- paste("sse run2fSSE.mod -clean=3 -sge_queue=lowprio.q -samples=",samples," -threads=5 -run_on_sge ", sep="")
    com2 <- paste("sse run2fSSE.mod -sge_queue=all.q@n27 -clean=3 -samples=",samples," -threads=5 -run_on_sge ", sep="")
    com3 <- paste("-directory=",directory,  "'",sep="")

  
  command <- paste(com1,com2,com3, collapse="")

  system(command, wait=TRUE)
  
  # Extract results
  res<-read.csv(paste(directory,"/raw_results_run2fSSE.csv",sep=""))
  result<-data.frame(nr=c(1:nrow(res)),name=name,description=description)
  
  result$reeCL <- res$TVCL -1
  result$reeV <- res$TVV -20
  result$reeEMAX <- res$EMAX -2
  result$reeE50 <- res$E50 -25
  result$reeHL <- res$HL - 5

  result$sdCL <- sd(res$TVCL)
  result$sdV <- sd(res$TVV)
  result$sdEMAX <- sd(res$EMAX)
  result$sdE50 <- sd(res$E50)
  result$sdHL <- sd(res$HL)

 
  meta<-list()
  meta$description <- description
  meta$ngroups     <- ngroups
  meta$nsubjects   <- prev[[1]]$nid
  meta$misspec     <- misspec
  meta$nsteps      <- nsteps

  save(result, prev, meta, summary, file=paste("result_",name,".Rdata",sep=""))
  
  return(result)

}
  

evalFixedDesign<-function(name, description,samples,
                          times=list(c(1,2,3),c(1,2,4), c(1,2,3)),
                          cov=list(20,30,40),
                          nid=list(10,10,10),
                          ngroups, nsteps,
                          misspec){

  prev <- list(list(dose = list(1000),
                    cov = cov,
                    nid = nid,
                    samplingtimes = times))
  
  result <- evalDesign(prev = prev,
                       name = name,
                       description = description,
                       samples = samples,
                       ngroups = ngroups,
                       nsteps = nsteps,
                       misspec = misspec)
 
  return(result)
}




compareDesigns<-function(resfiles=as.list(dir(pattern="result_"))){
  
  # make empty results data frames
  allBiasPrec<- data.frame()
  allTimes<- data.frame()
  allCov <- data.frame()
  allMeta<-data.frame()
  allMisp<-data.frame()
  resAUC<-data.frame()
  
  # iterate over different scenarios
  count=1
  prevdescription=NA
  for( file in resfiles  ) {    
    load(file)

    
    AUC<-result
    
    if(meta$description != prevdescription & !is.na(prevdescription)){
      count=1
      print('reset count')
    }
    prevdescription=meta$description

    # meta data
    ngroups<-meta$ngroups
    misspec<-meta$misspec
    nsteps<-meta$nsteps
    nsubject<-min(unique(meta$nsubjects[[1]]))
    nadult<-max(unique(meta$nsubjects[[1]]))
    misspec<-ifelse(is.na(misspec),"No",misspec)
    description<-meta$description    
    metadata<-data.frame(description, Groups=ngroups, name=count,
                         Steps=nsteps, Subjects=nsubject, Adults=nadult)    
    metadata<-melt(metadata, id.var=c("description","name"))    
    allMeta<-rbind(allMeta,metadata)

   # misspec
    good=c(2,25,5)
    ree=(good-meta$misspec)/good
    mis<-data.frame(name=count, description=description,
                    ngroups=meta$ngroups,
                    nsubject=min(unique(meta$nsubjects[[1]])),
                    reEmax=ree[1], reE50=ree[2], reHill=ree[3])
    mis<-melt(mis, id.var=c("description","name","ngroups","nsubject"))
    mis$xmin<-ifelse(mis$value<0, mis$value, 0)
    mis$xmax<-ifelse(mis$value>0, mis$value, 0)
    mis$x<-0
    allMisp<-rbind(allMisp, mis)
    
    # SSE
    result <- melt(result, c("name", "description", "nr"))
    result$name<-count


    
   # result <- cbind(result, metadata[rep(1,nrow(result)),])    
    allBiasPrec <- rbind(allBiasPrec, result)


    AUC$cl=  AUC$reeCL+1
    AUC$emax=AUC$reeEMAX+2
    AUC$e50=AUC$reeE50+25
    AUC$hill=AUC$reeHL+5

    for(i in 1:70){
      wt=i
      trueAUC=1+ (2 * wt^5)/ (25^5 + wt^5)
      obsAUC=AUC$cl+ (AUC$emax * wt^AUC$hill)/ (AUC$e50^AUC$hill + wt^AUC$hill)
      med=median(obsAUC)
      qlo=quantile(obsAUC,.25)
      qhi=quantile(obsAUC,.75)
      temp<-data.frame(description=meta$description,
                       name=count,
                       ngroups=meta$ngroups,
                       nsubject=min(unique(meta$nsubjects[[1]])),
                       wt=i,
                       med=med,
                       qlo=qlo,
                       qhi=qhi,
                       true=trueAUC)
      temp<-melt(temp, id.var=c("description","name","ngroups","nsubject","wt"))
      resAUC<-rbind(resAUC, temp)
    }
      
      
      
    
   # wt=1
   # AUC$AUC1=AUC$cl+ (AUC$emax * wt^AUC$hill)/ (AUC$e50^AUC$hill + wt^AUC$hill)
   # AUC$reeAUC1=(AUC$AUC1-trueAUC1)/trueAUC1
   # wt=10
   # AUC$AUC10=AUC$cl+ (AUC$emax * wt^AUC$hill)/ (AUC$e50^AUC$hill + wt^AUC$hill)
   # AUC$reeAUC10=(AUC$AUC10-trueAUC10)/trueAUC1
   # wt=20
   # AUC$AUC20=AUC$cl+ (AUC$emax * wt^AUC$hill)/ (AUC$e50^AUC$hill + wt^AUC$hill)
    
    
    # Sampling times
    stimes <- as.data.frame( prev[[1]]$samplingtimes)
    ngroups <- ncol(stimes)
    stimes$n <- 1:nrow(stimes)
    stimes$description <- meta$description
    stimes$nsubject<-min(unique(meta$nsubjects[[1]]))
    stimes$ngroups<-meta$ngroup
    names(stimes) <- c(seq(1:ngroups), "n", "description","nsubject","ngroups")
    stimes <- melt(stimes, id.var=c("n", "description","nsubject","ngroups"))
    names(stimes) <- c("n","description", "nsubject","ngroups","group", "time")
    stimes$group<-as.character(as.numeric(stimes$group))    
    stimes$name <- count
   # stimes<-  cbind(stimes, metadata[rep(1,nrow(stimes)),])    
    allTimes<- rbind(allTimes, stimes)
    
    # Covariates
    covs <- as.data.frame( prev[[1]]$cov)
    ngroups <- ncol(covs)
    covs$n <- 1:nrow(covs)
    covs$description<-meta$description
    covs$ngroups<-meta$ngroup
    covs$nsubject<-min(unique(meta$nsubjects[[1]]))
    names(covs) <- c(seq(1:ngroups), "n","description","ngroups","nsubject")
    covs <- melt(covs, id.var=c("n","description","ngroups","nsubject"))
    covs$name <- count
 #   covs <- cbind(covs,  metadata[rep(1,nrow(covs)),])    
    allCov <- rbind(allCov, covs)

    count=count+1
  }

  # generate numeric values for each scenario group, for x-axis


  for(i in unique(allMeta$description) ){
    nrSub<-length(unique(allMeta$name[allMeta$description == i])) # nr sub scenarios
    count=1
    for(l in unique(allMeta$name[allMeta$description == i]) ) {
      allMeta$xax[allMeta$description == i & allMeta$name== l] <- count
      count=count+1
    }
  }

  for(i in unique(allBiasPrec$description) ){
    nrSub<-length(unique(allBiasPrec$name[allBiasPrec$description == i])) # nr sub scenarios
    count=1
    for(l in unique(allBiasPrec$name[allBiasPrec$description == i]) ) {
      allBiasPrec$xax[allBiasPrec$description == i & allBiasPrec$name== l] <- count
      count=count+1
    }
  }

  allCov$label<-"Covariate values"
  for(i in unique(allCov$description) ){
    nrSub<-length(unique(allCov$name[allCov$description == i])) # nr sub scenarios
    count=1
    for(l in unique(allCov$name[allCov$description == i]) ) {
      allCov$xax[allCov$description == i & allCov$name== l] <- count
#      allCov$group[allCov$description == i & allCov$name== l] <- 1:length(allCov$xax[allCov$description == i & allCov$name== l])
      count=count+1
    }
  }
  
  
  

 # SSE only relevant parameters
 allBiasPrec<-allBiasPrec[allBiasPrec$variable %in% c("reeE50","reeEMAX","reeHL"),]


  # Times: format  
  allTimes$des<-as.numeric(as.factor(allTimes$name))
  for(i in unique(allTimes$description) ){
    count=1
    for(l in unique(allTimes$name[allTimes$description==i])){
      ngroup<-length(unique(allTimes$group[allTimes$name==l & allTimes$description==i]))
      total<-ngroup*0.025
      center<-(total/2)+0.0125
      distance <- (as.numeric(allTimes$group[allTimes$name==l & allTimes$description==i])/40)
      yaxis<- count - center + distance
      allTimes$yaxis[ allTimes$name==l & allTimes$description==i ]<-yaxis
      count=count+1
    }
  }


  allTimes$label<-"Sampling times"
  allMisp$label="Misspecification"


# Bias

  polygBias<-data.frame()
  for(i in unique(allBiasPrec$description)){
    for(n in unique(allBiasPrec$name[allBiasPrec$description==i])){
      for(p in unique(allBiasPrec$variable[allBiasPrec$description==i & allBiasPrec$name==n])){

      minp<-quantile(allBiasPrec$value[allBiasPrec$description==i & allBiasPrec$variable==p  & allBiasPrec$name==n], .25)
      maxp<-quantile(allBiasPrec$value[allBiasPrec$description==i & allBiasPrec$variable==p  & allBiasPrec$name==n], .75)
      med<-quantile(allBiasPrec$value[allBiasPrec$description==i & allBiasPrec$variable==p  & allBiasPrec$name==n], .50)
#      name=unique(allBiasPrec$name[allBiasPrec$description==i & allBiasPrec$variable==p  & allBiasPrec$name==n])
      temp<-data.frame(name=n,
                       description=i,
                       variable=p,
                       med=as.numeric(med),
#                       x=c(minp, minp, maxp, maxp),                       
 #                      y=c(name-0.05, name+0.05, name-0.05, name+0.05))
                       x=c(minp, maxp, maxp, minp),                       
                       y=c(n-0.05, n-0.05, n+0.05, n+0.05))
  #   print(temp)
      polygBias<-rbind(polygBias, temp)      
    }
    }
  }
      

  polygTimes<-data.frame()
  for(i in unique(allTimes$description)){
    for(n in unique(allTimes$name[allTimes$description==i])){


      minp<-min(allTimes$yaxis[allTimes$description==i & allTimes$name==n])-0.05
      maxp<-max(allTimes$yaxis[allTimes$description==i &  allTimes$name==n])+0.05

      temp<-data.frame(name=n,
                       description=i,
                       x=c(-1,25,25,-1),
                       y=c(minp,minp,maxp,maxp))
      
      polygTimes<-rbind(polygTimes,temp)
    }    
  }

  polygMis<-data.frame()
  for(i in unique(allMisp$description)){
    for(n in unique(allMisp$name[allMisp$description==i])){
      for(p in unique(allMisp$variable[allMisp$description==i & allMisp$name==n])){

        minp<-allMisp$xmin[allMisp$description==i & allMisp$name==n  & allMisp$variable==p]
        maxp<-allMisp$xmax[allMisp$description==i & allMisp$name==n  & allMisp$variable==p]
        ngroups<-allMisp$ngroups[allMisp$description==i & allMisp$name==n  & allMisp$variable==p]
        nsubject<-allMisp$nsubject[allMisp$description==i & allMisp$name==n  & allMisp$variable==p]
        
        temp<-data.frame(name=n,                        
                         description=i,
                         variable=p,
                         ngroups=ngroups,
                         nsubject=nsubject,
                         x=c(minp, maxp, maxp, minp),                       
                         y=c(n-0.05, n-0.05, n+0.05, n+0.05))
        
        polygMis<-rbind(polygMis, temp)
      }
    }
  }


polygAUC<-data.frame()
for(i in unique(resAUC$description)){
  for(n in unique(resAUC$name[resAUC$description==i])){

    minp<-resAUC$value[resAUC$description==i & resAUC$name==n & resAUC$variable=="qlo"]
    maxp<-resAUC$value[resAUC$description==i & resAUC$name==n & resAUC$variable=="qhi"]
    wt<-resAUC$wt[resAUC$description==i & resAUC$name==n & resAUC$variable=="qhi"]
    ngroups<-resAUC$ngroups[resAUC$description==i & resAUC$name==n & resAUC$variable=="qhi"]
    nsubject<-resAUC$nsubject[resAUC$description==i & resAUC$name==n & resAUC$variable=="qhi"]
    
     temp<-data.frame(name=n,
                      description=i,                   
                      variable=p,
                      ngroups=ngroups,
                      nsubject=nsubject,
                      y=c(minp, rev(maxp)),
                      x=c(wt, rev(wt)))
                      
    polygAUC<-rbind(polygAUC, temp)
  }
}
      
  


 allCov$description<-factor(allCov$description, levels=levels(allMeta$description))

return(list(allCov=allCov, allMeta=allMeta, polygMis=polygMis, polygBias=polygBias,
            allTimes=allTimes, resAUC=resAUC, polygTimes=polygTimes, polygAUC=polygAUC))


}


getPars <-function(lstfile){

  parameter.list <- create.parameter.list(lstfile)

  #attach(parameter.list,warn.conflicts=F)  

  ## Set up matrix
  if(any(parameter.list$separval!="" & parameter.list$separval!=0)) {
    ret.mat <- matrix(0,
                      nrow=length(parameter.list$parval),
                      ncol=3,
                      dimnames=list(c(),c("Parameter","Value","RSE"))
                      )
    ret.mat[,1] <- parameter.list$parnam
    ret.mat[,2] <- parameter.list$parval
    ret.mat[,3] <- parameter.list$separval

  } else {
    ret.mat <- matrix(0,
                      nrow=length(parameter.list$parval),
                      ncol=2,
                      dimnames=list(c(),c("Parameter","Value"))
                      )
    ret.mat[,1] <- parameter.list$parnam
    ret.mat[,2] <- parameter.list$parval
  }

  class(ret.mat) <- "char.matrix"
  return(ret.mat)
}
