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
