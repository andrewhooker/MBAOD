#' combine current design and design space
#' 
#' combine initial design for current cohort and final design for all previous cohorts.  Make
#' design space for previous designs so that no optimization occurs, then combine with current design space of current cohort.
#' 

combine_design_and_space <- function (cohort, aod_res) {
  cur_design <- do.call(create_design,cohort$design) # for cohort
  
  prev_design <- combine_designs(aod_res)
  
  if(is.null(prev_design)){
    arg_list <- cohort$optimize$design_space
    arg_list$design=cur_design
    tot_space <-do.call(create_design_space,arg_list)
  } else{
    # design space
    prev_space <- create_design_space(prev_design)
    prev_design <- prev_space$design
    prev_space <- prev_space$design_space
    
    arg_list <- cohort$optimize$design_space
    arg_list$design=cur_design
    cur_space <-do.call(create_design_space,arg_list)
    cur_design <- cur_space$design
    cur_space <- cur_space$design_space
    
    if(!is.null(cur_space[["grouped_xt"]])) cur_space$grouped_xt <- cur_space$grouped_xt + max(prev_space$grouped_xt)
    if(!is.null(cur_space[["grouped_a"]])) cur_space$grouped_a <- cur_space$grouped_a + max(prev_space$grouped_a)
    if(!is.null(cur_space[["grouped_x"]])) cur_space$grouped_x <- cur_space$grouped_x + max(prev_space$grouped_x)
    
    
    # combine designs and design spaces (make into dataframes for rbind_list)
    cur_design_df <- lapply(cur_design,data.frame)
    cur_space_df <- lapply(cur_space,data.frame)
    prev_design_df <- lapply(prev_design,data.frame)
    prev_space_df <- lapply(prev_space,data.frame)
    
    tot_design <- lapply(do.call(function(...){mapply(dplyr::rbind_list,...,SIMPLIFY=FALSE)},
                                 list(prev_design_df,cur_design_df)),as.matrix)
    tot_design$m <- sum(tot_design$m)
    tot_design <- do.call(create_design,tot_design) 
    
    tot_space <- lapply(do.call(function(...){mapply(dplyr::rbind_list,...,SIMPLIFY=FALSE)},
                                list(prev_space_df,cur_space_df)),as.matrix)
    if(!is.null(tot_space[["use_grouped_a"]])) tot_space$use_grouped_a <- FALSE
    if(!is.null(tot_space[["use_grouped_x"]])) tot_space$use_grouped_x <- FALSE
    if(!is.null(tot_space[["use_grouped_xt"]])) tot_space$use_grouped_xt <- FALSE
    tot_space$maxtotni  <- sum(tot_space$maxtotni)
    tot_space$mintotni  <- sum(tot_space$mintotni)
    tot_space$maxtotgroupsize  <- sum(tot_space$maxtotgroupsize)
    tot_space$mintotgroupsize  <- sum(tot_space$mintotgroupsize)
    
    arg_list <- tot_space
    arg_list$design=tot_design
    tot_space <-do.call(create_design_space,arg_list)
  }
  return(tot_space)
}


