merge_parameters <- function (prev_est, new_parameters=NULL, manipulation=new_parameters$manipulation) {
  bpop <- c(prev_est$thetas,new_parameters$bpop) # just append the new values assuming order is correct
    
  d <- NULL
  if(!is.null(prev_est$omega)){
    omega <- matrix_from_triangle(prev_est$omega)
    d <- diag(omega)
    covd <- omega[lower.tri(omega)]
    if(all(covd==0)) covd <- NULL
  }
  if(!is.null(new_parameters$d)){
    if(size(new_parameters$d,1)==1 && !is.matrix(new_parameters$d)){ # we have just the diagnonal parameter values 
      d_new <- new_parameters$d
      d <- c(d,d_new)
    } else {
      stop("This method of using d not supported yet")
    }   
  }
  
  sigma <- NULL
  if(!is.null(prev_est$sigma)){
    sigma <- matrix_from_triangle(prev_est$sigma)
  }
  if(!is.null(new_parameters$sigma)){
    sigma_new <- new_parameters$sigma
    if(size(new_parameters$sigma,1)==1 && !is.matrix(new_parameters$sigma)){ # we have just the diagnonal parameter values 
      sigma_new <- diag(new_parameters$sigma,size(new_parameters$sigma,2),size(new_parameters$sigma,2))
    }    
    sigma <- Matrix::bdiag(sigma,sigma_new)
  }
  
  if(!is.null(manipulation)){ 
    for(i in 1:length(manipulation)) eval(manipulation[[i]])
  }
  
  return(list(sigma=sigma,bpop=bpop,d=d) )
}