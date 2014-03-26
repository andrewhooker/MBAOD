readEstPar <- function(reserr="comb",     # 1 or 2 sigmas
                       mpar=c(2, 25, 5),  # maturation parameters (can be misspecified)
                       simple=FALSE,
                       numeric=FALSE,# indicates if mpar has to be used
                       out.lst){      
  
  # Function to extract parameter estimates from LST file as input for function_input file
  
  
  print(' - Extracting current parameter estimates ')
  
  # Read LST file (full or reduced)
  if(file.exists(out.lst["reduced"])) { f <- file(out.lst["reduced"]); d <- readLines(f); close(f) }
  if(file.exists(out.lst["full"])) { f <- file(out.lst["full"]); d <- readLines(f); close(f) }
  
  # Find lines 
  lineTh  <- grep("THETA - VECTOR OF FIXED EFFECTS PARAMETERS",d)[1]+5
  lineOm1 <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",d)[1]+6
  lineOm2 <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",d)[1]+9
  lineSi  <- grep (" SIGMA - COV MATRIX FOR RANDOM EFFECTS",d)[1]+6
  if(reserr == "comb") lineSi2<-grep (" SIGMA - COV MATRIX FOR RANDOM EFFECTS",d)[1]+9
  
  # Read parameter values
  th <- as.numeric(unlist(strsplit(d[lineTh], " "))[c(10,12,14,16, 18)])
  om <- c(as.numeric(substr( d[lineOm1],2, 20)), as.numeric(unlist(strsplit(substr( d[lineOm2],2, 30), "  "))[6]))
  si <- as.numeric(substr(d[lineSi],10,20))
  if(reserr == "comb") si <- c(as.numeric(substr(d[lineSi],10,20)), as.numeric(substr(d[lineSi2],20,40)))
  
  
  # Read parameter values
  th <- as.numeric(unlist(strsplit(d[lineTh], " "))[c(10,12,14,16, 18)])
  om <- c(as.numeric(substr( d[lineOm1],2, 20)), as.numeric(unlist(strsplit(substr( d[lineOm2],2, 30), "  "))[6]))
  si <- as.numeric(substr(d[lineSi],10,20))
  if(reserr == "comb") si <- c(as.numeric(substr(d[lineSi],10,20)), as.numeric(substr(d[lineSi2],20,40)))
  
  # Define Theta
  if(!is.na(th[5])){
    estTh<-paste("0 "  , th[1] , "0; 0", th[2], "0; 0 ", th[3]   , "0; 0", th[4]   , "0; 0", th[5]  , "0")
  }
  if( is.na(th[5]) || simple==TRUE ){
    estTh <- paste("0 ", th[1] - mpar[1] , "0; 0", th[2], "0; 0 ", mpar[1] , "0; 0", mpar[2] , "0; 0", mpar[3], "0")
  }
  
  
  # Define Omega
  estOm<-paste("0 ", om[1], "0; 0", om[2], "0")
  # Define Sigma  
  if(length(si) == 1) estSig <- si[1]
  if(length(si) == 2) estSig <- paste(si[1], "0 ; 0", si[2]) # covariance matrix
  
  if( is.na(th[1]) | is.na(si[1]) | is.na(si[2])){
    estTh <- paste("0 ", 1 , "0; 0", 20 , "0; 0 ", mpar[1] , "0; 0", mpar[2] , "0; 0", mpar[3], "0")
    estOm<-paste("0 ",.05, "0; 0", .05, "0")
    estSi<- paste(0.015, "0 ; 0", 0.0015) 
  }
  
  # Return parameter estimates
  if(numeric==FALSE){
    return(list(estTh=estTh,
                estOm=estOm,
                estSig=estSig))
  }
  if(numeric==TRUE){
    return(list(th=th, om=om, si=si))
  }    
}
