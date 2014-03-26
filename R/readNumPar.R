readNumPar <- function(full_est_mod){
  # Function to determine total number of parameters (for first run this may be a reduced model, i.e. incorrect number
  print(' - Reading total number of parameters')
  
  ################################################################
  # Read parameters full model to determine total number of parameters
  o <- file(full_est_mod)
  f <- readLines(o)
  close(o)
  
  # find lines parameter estimates
  lineTh <- grep("THETA", f); lineTh<-lineTh[length(lineTh)]+1
  lineOm <- grep("OMEGA", f)+1
  lineSi <- grep("SIGMA", f)+1
  lineEr <- grep("ERROR", f)+1
  
  # find parameters
  th<-as.numeric(unlist(lapply(strsplit(f[lineTh:(lineOm-2)],","),
                               function(x) return(x[2]))))
  om<-as.numeric(f[lineOm:(lineSi-2)])
  si<-as.numeric(f[lineSi:(lineEr-2)])
  
  # return parameter estimates
  return(list(nth=length(th), nom=length(om), nsig=length(si)))
}
