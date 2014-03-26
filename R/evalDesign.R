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

