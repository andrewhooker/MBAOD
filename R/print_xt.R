print_xt <- function (xtopt, ni, model_switch,fn="",head_txt="Optimized xt values:\n",xt_other=NULL,precision="%6.3g") {
  cat(head_txt,file=fn)
  for(j in 1:size(xtopt,1)){
    xtopt_i = xtopt[j,1:ni[j]]
    model_switch_i = model_switch[j,1:ni[j]]
    if(!is.null(xt_other)) xt_other_i = xt_other[j,1:ni[j]]
    for(i in unique(as.vector(model_switch_i))){
      xtopt_i_sort = sort(xtopt_i[model_switch_i==i])
      if(!is.null(xt_other)) xt_other_i_sort = xt_other_i[order(xtopt_i[model_switch_i==i])]
      if(length(unique(as.vector(model_switch_i)))>1) cat(sprintf("Model %g : ", i),file=fn)
      #if(size(xtopt,1)>1) cat(sprintf("Group %g : ", j),file=fn)
      cat(sprintf("Group %g : ", j),file=fn)
      if(!is.null(xt_other)) {
        cat(sprintf(precision, xt_other_i_sort),file=fn)
      } else {
        cat(sprintf(precision, xtopt_i_sort),file=fn)
      }
      cat("\n",file=fn)
    }
  }
  invisible()
}



