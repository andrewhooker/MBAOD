matrix_from_triangle <- function(list_of_elements,row_order=T){
  X <- diag(length(list_of_elements)) 
  if(row_order) X[upper.tri(X, diag=TRUE)] <- unlist(list_of_elements) 
  if(!row_order) X[lower.tri(X, diag=TRUE)] <- unlist(list_of_elements) 
  X <- X + t(X) - diag(diag(X)) 
  return(X)
}