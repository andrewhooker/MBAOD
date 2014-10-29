plot_parameter_estimates <- function(results_all,
                                     true_values,
                                     thetas=T,
                                     omegas=T,
                                     sigmas=T,
                                     REE=T,
                                     use_sd=T,
                                     scales="free",
                                     zero_line=F){
  parameters <- 
    results_all$est_summary_total[!duplicated(results_all$est_summary_total$iter, 
                                              fromLast = TRUE),]
  par_filter <- c()
  if(thetas) par_filter <- c(par_filter, grep("^THETA",names(parameters),value=TRUE))
  if(omegas && !use_sd) par_filter <- c(par_filter, grep("^OMEGA_var",names(parameters),value=TRUE))
  if(omegas && use_sd) par_filter <- c(par_filter, grep("^OMEGA_sd",names(parameters),value=TRUE))
  if(sigmas && !use_sd) par_filter <- c(par_filter, grep("^SIGMA_var",names(parameters),value=TRUE))
  if(sigmas && use_sd) par_filter <- c(par_filter, grep("^SIGMA_sd",names(parameters),value=TRUE))
  
  parameters <- parameters[par_filter]
  
  
  #   parameters <- parameters[c(grep("^THETA",names(parameters),value=TRUE),
  #                              grep("^OMEGA_var",names(parameters),value=TRUE),
  #                              grep("^SIGMA_var",names(parameters),value=TRUE))]
  #   
  if(length(parameters)!=length(true_values)){ 
    cat("Length pf parameters and true_vales not the same,\n",
        "plotting parameter values instead of REE\n")
    REE=F
    true_values <- 0
  }
  
  if(REE){ 
    parameters_ree <- data.frame(t(apply(parameters,1,function(x,true_vales){(x-true_values)/true_values*100},true_values)))
    ylb <- "REE (%)"
  } else {
    parameters_ree <- data.frame(t(apply(parameters,1,function(x,true_vales){(x-true_values)},true_values)))
    ylb <- "EE"
    if(all(true_values==0)) ylb <- "Parameter value"
  }
  stacked_parameters_ree <- stack(parameters_ree)
  stacked_parameters_ree$type <- "Random Effects"
  stacked_parameters_ree[grep("THETA",stacked_parameters_ree$ind),"type"] <- "Fixed Effects"
    
  p <- ggplot(data=stacked_parameters_ree,aes(x=ind,y=values)) 
  p <- p +  geom_boxplot() + geom_jitter(position=position_jitter(width=0.05)) + 
    xlab("Parameter") +
    ylab(ylb) +
    facet_wrap(~type,scales=scales) +
    theme(title = element_text(size=25),
          axis.title = element_text(size=20),
          axis.text.y = element_text(size=20),
          strip.text=element_text(size=20),
          axis.text.x = element_text(angle = 45, hjust = 1,size=15))
  if(zero_line) p <- p + geom_hline(aes(yintercept=0,color="red"))
  return(p)
}