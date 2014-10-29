extract_cohort_design <- function(optimized_design,cohort){
  cohort_design <- do.call(create_design,cohort$design) # for cohort
  cohort_design_opt <- cohort_design
  
  if(!is.null(cohort_design_opt[["xt"]])) cohort_design_opt$xt <- optimized_design$xt[(optimized_design$m - cohort_design_opt$m +1):optimized_design$m,,drop=F]
  if(!is.null(cohort_design_opt[["x"]])) cohort_design_opt$x <- optimized_design$x[(optimized_design$m - cohort_design_opt$m+1):optimized_design$m,,drop=F]
  if(!is.null(cohort_design_opt[["a"]])) cohort_design_opt$a <- optimized_design$a[(optimized_design$m - cohort_design_opt$m+1):optimized_design$m,,drop=F]
  if(!is.null(cohort_design_opt[["ni"]])) cohort_design_opt$ni <- optimized_design$ni[(optimized_design$m - cohort_design_opt$m+1):optimized_design$m,,drop=F]
  if(!is.null(cohort_design_opt[["groupsize"]])) cohort_design_opt$groupsize <- optimized_design$groupsize[(optimized_design$m - cohort_design_opt$m+1):optimized_design$m,,drop=F]
  return(cohort_design_opt)
}      