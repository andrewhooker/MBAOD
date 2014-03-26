
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

