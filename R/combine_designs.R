combine_designs <- function (design_list,design_name="design") {
  
  if(length(design_list)==0) return(NULL)
  if(is.null(design_list)) return(NULL)
  
  # create full designs from user inputed designs
  full_design_list <- lapply(mapply("[[",design_list,design_name,SIMPLIFY=FALSE),function(x){do.call(create_design,x)})
  
  # make each element of each design into a dataframe for rbind_list
  full_design_list_df <- lapply(full_design_list,function(x){lapply(x,data.frame)}) 
  
  # merge the list of designs 
  if(packageVersion("dplyr") >= "0.5.0"){
    merged_design <- lapply(do.call(function(...){mapply(dplyr::bind_rows,...,SIMPLIFY=FALSE)},full_design_list_df),as.matrix) 
  } else {
    merged_design <- lapply(do.call(function(...){mapply(dplyr::rbind_list,...,SIMPLIFY=FALSE)},full_design_list_df),as.matrix) 
  }
  merged_design$m <- sum(merged_design$m)
  
  # check for errors and add column and row names
  merged_design <- do.call(create_design,merged_design) 
  
  return(merged_design)
}