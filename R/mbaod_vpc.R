mbaod_vpc <- function (design, 
                       model, 
                       parameters_true, 
                       results_all, 
                       separate.groups=F,
                       model_num_points=100,
                       ...) {
  library(Hmisc)
  poped.db_true <- do.call(create.poped.database,c(design,model,parameters_true))
  plot_true <- plot_model_prediction(poped.db_true,y_lab="Concentration",sample.times = F,
                                     IPRED=T,
                                     IPRED.lines.pctls=T,
                                     separate.groups=separate.groups,
                                     model_num_points=model_num_points)
  
  mp_summary_all <- c()
  for(i in grep("^iteration",names(results_all))){
    i=1
    it_res <- results_all[[i]]
    est_result <- it_res[[max(grep("^cohort",names(it_res)))]][["est_result"]]
    parameters <- merge_parameters(est_result)
    poped.db <- do.call(create.poped.database,c(design,model,parameters))
    mp <- model_prediction(poped.db,IPRED=T,model_num_points=model_num_points)
    mp_summary <- with(mp,{summarize(IPRED,llist(Time),smedian.hilow)})
    if(separate.groups){
      mp_summary <- with(mp,{summarize(IPRED,llist(Time,Group),smedian.hilow)})
      levels(mp_summary$Group) <- paste("Group:",levels(mp_summary$Group))
    }
    mp_summary_all <- rbind(mp_summary_all,mp_summary)
  }
  
  p <- plot_true + 
    stat_summary(data=mp_summary_all,
                 aes(x=Time,y=IPRED),
                 geom="ribbon",
                 fun.data="median_hilow",
                 alpha=0.5,
                 fill="red") +
    stat_summary(data=mp_summary_all,
                 aes(x=Time,y=Lower),
                 geom="ribbon",
                 fun.data="median_hilow",
                 alpha=0.5,
                 fill="blue") +
    stat_summary(data=mp_summary_all,
                 aes(x=Time,y=Upper),
                 geom="ribbon",
                 fun.data="median_hilow",
                 alpha=0.5,
                 fill="blue")
  return(p)
}