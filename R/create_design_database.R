create_design_database <- function (tot_design, tot_space, parameters, cohort) {
  tot_space_tmp <- tot_space
  if(!is.null(tot_space_tmp[["use_grouped_a"]])){
    tot_space_tmp$bUseGrouped_a <- tot_space_tmp[["use_grouped_a"]]
    tot_space_tmp[["use_grouped_a"]] <- NULL
  }
  if(!is.null(tot_space_tmp[["use_grouped_x"]])){
    tot_space_tmp$bUseGrouped_x <- tot_space_tmp[["use_grouped_x"]]
    tot_space_tmp[["use_grouped_x"]] <- NULL
  }
  if(!is.null(tot_space_tmp[["use_grouped_xt"]])){
    tot_space_tmp$bUseGrouped_xt <- tot_space_tmp[["use_grouped_xt"]]
    tot_space_tmp[["use_grouped_xt"]] <- NULL
  }
  if(!is.null(tot_space_tmp[["grouped_a"]])){
    tot_space_tmp$G_a <- tot_space_tmp[["grouped_a"]]
    tot_space_tmp[["grouped_a"]] <- NULL
  }
  if(!is.null(tot_space_tmp[["grouped_x"]])){
    tot_space_tmp$G_x <- tot_space_tmp[["grouped_x"]]
    tot_space_tmp[["grouped_x"]] <- NULL
  }
  if(!is.null(tot_space_tmp[["grouped_xt"]])){
    tot_space_tmp$G_xt <- tot_space_tmp[["grouped_xt"]]
    tot_space_tmp[["grouped_xt"]] <- NULL
  }
  tot_space_tmp$maxni <- max(tot_space_tmp$maxni)
  tot_space_tmp$minni <- min(tot_space_tmp$minni)
  tot_space_tmp$maxtotni <- NULL
  tot_space_tmp$mintotni <- NULL
  
  #FIXED the mismatched of design space for discrete variables##
  tot_space_tmp$discrete_x  <- tot_space_tmp$x_space
  tot_space_tmp$x_space <- NULL
  
  tot_space_tmp$discrete_a  <- tot_space_tmp$a_space
  tot_space_tmp$a_space <- NULL
  ##############################################################
  
  poped.db <- do.call(create.poped.database,c(cohort$optimize$model,
                                              tot_space_tmp,
                                              tot_design,
                                              parameters,
                                              cohort$optimize$settings.db))
}