makeSimFile_nonmem <- function(prev, step=1, last=FALSE, first=FALSE, fixed=FALSE){
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
    ngrp <- length(nid)
    wt    <- unlist(prev[[step]]$cov)[(maxwt-ngrp+1):maxwt]
    times.all <- prev[[step]]$samplingtimes[(maxtimes-ngrp+1):maxtimes]
          
     # generate records per ID
    for(j in 1:ngrp){
      times <- unlist(times.all[j])
      times <- round(sort(times),3)
      for(i in 1:nid[j]){
        idose <- round(dose*(wt[j]/70),3)
        obsi  <- data.frame(id=i, time=times, dv=NA, amt=NA, wt=wt[j])
        dosi  <- data.frame(id=i, time=0, dv=NA, amt=idose, wt=wt[j])
        idat  <- rbind(dosi, obsi)
        dat   <- rbind(dat, idat)
      } # end forloop ID
    }
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
