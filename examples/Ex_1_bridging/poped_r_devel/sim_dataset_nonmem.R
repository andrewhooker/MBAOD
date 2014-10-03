sim_dataset_nonmem <- function(poped.db,
                               models_to_use="all",
                               model_num_points=NULL,
                               model_minxt=NULL,model_maxxt=NULL,
                               groups_to_use="all",
                               IPRED=F){
  
  
  design = poped.db$design
  design$xt <- poped.db$gxt
  design$x <- poped.db$gx
  design$a <- poped.db$ga
  
  used_times <- 0*design$xt
  for(i in 1:size(design$xt,1)) used_times[i,1:design$ni[i]] <- 1
  
  if(all(groups_to_use=="all")){
    groups_to_use = 1:size(design$xt,1)
  }
  if(all(models_to_use=="all")){
    #models_to_use = unique(as.vector(design$model_switch))
    models_to_use = unique(as.vector(design$model_switch[used_times==1]))
  }
  
  df <- data.frame()
  
  for(i in 1:length(groups_to_use)){
    if((isempty(design$a))){
      a_i = zeros(0,1)
    } else {
      a_i = design$a[groups_to_use[i],,drop=F]
    }
    if((isempty(design$x))){
      x_i = zeros(0,1)
    } else {
      x_i = design$x[groups_to_use[i],,drop=F]
    }
    
    
    xt_i = design$xt[groups_to_use[i],1:design$ni[groups_to_use[i]]]
    model_switch_i = design$model_switch[groups_to_use[i],1:design$ni[groups_to_use[i]]]
    if(!all(models_to_use == unique(as.vector(design$model_switch[used_times==1])))){ ## needs testing
      xt_i = xt_i[model_switch_i %in% models_to_use]
      model_switch_i = model_switch_i[model_switch_i %in% models_to_use]
    }
    
    group.df <- data.frame(Time=xt_i,DV=NA,Group=groups_to_use[i],Model=model_switch_i)
    if(!isempty(a_i)) group.df <- data.frame(group.df,a_i)
    if(!isempty(x_i)) group.df <- data.frame(group.df,x_i)
    
    #     group.df <- data.frame(Time=xt_i,PRED=drop(pred[[1]]),Group=groups_to_use[i],
    #                            ##paste("Group",i,sep="_"),
    #                            Model=model_switch_i)
    #     
    
    browser()
    
    group.df <- merge(group.df,data.frame(ID=1:design$groupsize[groups_to_use[i]]))
    group.df["ID"]
    group.df.ipred <- data.frame()
    for(j in 1:design$groupsize){
      tmp.df <- group.df
      bocc_stop=bocc_start + poped.db$NumOcc - 1
      if(nrow(fulldocc)==0){ 
        bocc_start=0
        bocc_stop=0
      }
      fg_sim = feval(poped.db$fg_pointer,x_i,a_i,design$bpop[,2,drop=F],b_sim_matrix[j,],t(bocc_sim_matrix[bocc_start:bocc_stop,]))
      bocc_start = bocc_stop + 1
      ipred <- feval(poped.db$ff_pointer,model_switch_i,xt_i,fg_sim,poped.db)
      ipred <- drop(ipred[[1]])
      ID <- (i-1)*num_ids+j
      tmp.df["ID"] <- ID
      tmp.df["IPRED"] <- ipred
      
      if(DV){
        eps_sim = rmvnorm(length(xt_i),sigma=design$sigma)
        dv <- feval(poped.db$ferror_pointer,model_switch_i,xt_i,fg_sim,eps_sim,poped.db) 
        dv <- drop(dv[[1]])
        tmp.df["DV"] <- dv
      }
      
      group.df.ipred <- rbind(group.df.ipred,tmp.df)
    }
    group.df <- group.df.ipred
  }
  
  df <- rbind(df,group.df)
  #model.pred  <- rbind(model.pred,i=returnArgs[[1]])
  #model.pred[paste("Group",i,sep="_")]  <- returnArgs[[1]]
  
  df$Group <- as.factor(df$Group)
  df$Model <- as.factor(df$Model)
  if(IPRED) df$ID <- as.factor(df$ID)
  return( df ) 
}