

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
