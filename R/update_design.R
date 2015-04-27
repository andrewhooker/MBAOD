update_design <- function(tot_design,opt_output){
  optimized_design <- tot_design
  if(packageVersion("PopED") < 0.2){
    if(!is.null(optimized_design[["xt"]])) optimized_design$xt[,] <- opt_output$poped.db$gxt[,]
    if(!is.null(optimized_design[["x"]])) optimized_design$x[,] <- opt_output$poped.db$gx[,]
    if(!is.null(optimized_design[["a"]])) optimized_design$a[,] <- opt_output$poped.db$ga[,] 
    if(!is.null(optimized_design[["ni"]])) optimized_design$ni[,] <- opt_output$poped.db$gni[,]
    if(!is.null(optimized_design[["groupsize"]])) optimized_design$groupsize[,] <- opt_output$poped.db$groupsize[,]
  } else {
    if(!is.null(optimized_design[["xt"]])) optimized_design$xt[,] <- opt_output$poped.db$design$xt[,]
    if(!is.null(optimized_design[["x"]])) optimized_design$x[,] <- opt_output$poped.db$design$x[,]
    if(!is.null(optimized_design[["a"]])) optimized_design$a[,] <- opt_output$poped.db$design$a[,] 
    if(!is.null(optimized_design[["ni"]])) optimized_design$ni[,] <- opt_output$poped.db$design$ni[,]
    if(!is.null(optimized_design[["groupsize"]])) optimized_design$groupsize[,] <- opt_output$poped.db$design$groupsize[,]
  }
  #optimized_design <- do.call(create_design,optimized_design) 
  return(optimized_design)
}