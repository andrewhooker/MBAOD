


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
