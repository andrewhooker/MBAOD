#' Model predictions 
#' 
#' Function generates model predictions for the typical value in the population,
#' individual predictions and data predictions.
#' 
#' @inheritParams RS_opt
#' @param models_to_use Which model number should we use?
#' @param model_num_points How many points should be plotted.  If not a number then the design in poped.db is used.
#' @param model_minxt The minimum of the sample times for the predictions.
#' @param model_maxxt The maximum of the sample times for the predictions.
#' @param include_sample_times Should the sample times from poped.db be included in the predictions?
#' @param IPRED Should we simulate individual predictions?
#' @param DV should we simulate observations?
#' @param num_ids The number of individuals per group to simulate if using IPRED or DV.
#' @param groups_to_use Which groups should we use for predictions from the poped.db.
#' @return A dataframe of simulated data, either with some dense grid of samples or based on the design in the poped
#' database.
#' 
#' @family evaluate_design
#' @family Simulation
#' 
#' @example tests/testthat/examples_fcn_doc/examples_model_prediction.R
#' 

#' 
## allow for input not from poped.db
model_prediction <- function(poped.db=NULL,
                             models_to_use="all",
                             model_num_points=NULL,
                             model_minxt=NULL,
                             model_maxxt=NULL,
                             include_sample_times=T,
                             groups_to_use="all",
                             IPRED=FALSE,
                             DV=FALSE,
                             include_a = TRUE,
                             include_x = TRUE,
                             dosing=NULL, # otherwise a list of lists with "Time" and dosing columns needed for each group
                             filename=NULL,
                             design=list( ## passed to check_design
                               xt=poped.db[["gxt"]], ## -- Matrix defining the initial sampling schedule row major, can also be a list of vectors--
                               groupsize=poped.db$design$groupsize,  ## -- Vector defining the size of the different groups (num individuals in each group) --
                               m=poped.db[["m"]], ## -- Number of groups, computed from xt if not defined --                    
                               x=poped.db[["gx"]], ## -- Matrix defining the initial discrete values --               
                               a=poped.db[["ga"]], ## -- Vector defining the initial covariate values --        
                               ni=poped.db$design$ni,    ## -- Vector defining the number of samples for each group, computed as all elements of xt by default --
                               model_switch=poped.db$design$model_switch),
                             parameters=list(
                               docc=poped.db$design$docc,
                               d = poped.db$design$d,
                               bpop = poped.db$design$bpop,
                               covd = poped.db$design$covd,
                               covdocc = poped.db$design$covdocc,
                               sigma = poped.db$design$sigma),
                             models = list(
                               fg_pointer=poped.db$fg_pointer,
                               ff_pointer=poped.db$ff_pointer,
                               ferror_pointer= poped.db$ferror_pointer
                             ),
                             predictions=NULL,
                             NumOcc=poped.db$NumOcc,
                             maxxt=poped.choose(poped.db$design$maxxt,xt), ## -- Matrix defining the max value for each sample --   
                             minxt=poped.choose(poped.db$design$minxt,xt)) ## -- Matrix defining the min value for each sample --
{
  
  if(is.null(predictions)){
    predictions=FALSE
    if(!is.null(poped.db) || (!is.null(unlist(parameters))) && !is.null(unlist(models)) && !is.null(unlist(design))) predictions=TRUE
  }
  
  if(is.null(poped.db) && is.null(unlist(design))) stop("Either 'poped.db' or 'design' need to be defined")
  
  design <- do.call(check_design,design)
  
  with(design,{
    
  #xt <- design$xt
  #ni <- design$ni
  
    
    
    if(DV) IPRED=T
    
    
    #design = poped.db$design
    #xt <- poped.db$gxt
    #x <- poped.db$gx
    #a <- poped.db$ga
    
    
    
    #--- size checking --------
    
    
    if(!is.null(dosing)){
      if(!length(dosing)==m){
        if(length(dosing) == 1) {
          dosing <- rep(dosing,m)
        } else {
          stop("dosing argument does not have the right dimensions.  
           Must be 1 list or a list of lists the size of the number of groups")
        }
      }
    }
    
    if(predictions){
      docc_size = 0
      if((!isempty(parameters$docc[,2]))){
        docc_size = size(parameters$docc[,2,drop=F],1)
      }
      d_size = 0
      if((!isempty(parameters$d[,2]))){
        d_size = size(parameters$d[,2,drop=F],1)
      }
    }
    
    used_times <- 0*xt
    for(i in 1:size(xt,1)) used_times[i,1:ni[i]] <- 1
    
    if(all(groups_to_use=="all")){
      groups_to_use = 1:size(xt,1)
    }
    if(all(models_to_use=="all")){
      #models_to_use = unique(as.vector(model_switch))
      models_to_use = unique(as.vector(model_switch[used_times==1]))
    }
    
    df <- data.frame()
    id_num_start <- 1
    for(i in 1:length(groups_to_use)){
      if(!exists("a")){
        a_i = zeros(0,1)
      } else if((isempty(a))){
        a_i = zeros(0,1)
      } else {
        a_i = a[groups_to_use[i],,drop=F]
      }
      if(!exists("x")){
        x_i = zeros(0,1)
      } else if((isempty(x))){
        x_i = zeros(0,1)
      } else {
        x_i = x[groups_to_use[i],,drop=F]
      }
      num_ids = groupsize[groups_to_use[i]]
      
      if(all(is.null(model_num_points))){
        xt_i = xt[groups_to_use[i],1:ni[groups_to_use[i]]]
        model_switch_i = model_switch[groups_to_use[i],1:ni[groups_to_use[i]]]
        if(!all(models_to_use == unique(as.vector(model_switch[used_times==1])))){ ## needs testing
          xt_i = xt_i[model_switch_i %in% models_to_use]
          model_switch_i = model_switch_i[model_switch_i %in% models_to_use]
        }
      } else {
        xt_i <- c()
        model_switch_i <- c()
        if(length(models_to_use)>1 && length(model_num_points)==1) model_num_points <- rep(model_num_points,length(models_to_use))
        for(j in models_to_use){
          if(is.null(model_minxt)){
            minv <- min(as.vector(minxt[model_switch==j])) 
          } else {                    
            minv = model_minxt[j]
          }
          if(is.null(model_maxxt)){
            maxv <- max(as.vector(maxxt[model_switch==j])) 
          } else {
            maxv = model_maxxt[j]
          }                #xt = t(seq(minv,maxv,length.out=model_num_points[i]))
          
          xt_i= c(xt_i,seq(minv,maxv,length.out=model_num_points[j]))
          
          #model.pred <- rbind(xt)
          #model.pred <- data.frame(Time=xt)
          #model.pred <- c(model.pred,foo=xt)
          #browser()
          model_switch_i = c(model_switch_i,j*matrix(1,1,model_num_points[j]))
        }
        if(include_sample_times){
          xt_i_extra = xt[groups_to_use[i],1:ni[groups_to_use[i]]]
          model_switch_i_extra = model_switch[groups_to_use[i],1:ni[groups_to_use[i]]]
          if(!all(models_to_use == unique(as.vector(model_switch[used_times==1])))){ ## needs testing
            xt_i_extra = xt_i_extra[model_switch_i_extra %in% models_to_use]
            model_switch_i_extra = model_switch_i_extra[model_switch_i_extra %in% models_to_use]
          }
          tmp.include <- !(xt_i_extra %in% xt_i)
          xt_i <- c(xt_i,xt_i_extra[tmp.include])
          model_switch_i <- c(model_switch_i,model_switch_i_extra[tmp.include])
          tmp.order <- order(xt_i)
          xt_i <- xt_i[tmp.order]
          model_switch_i <- model_switch_i[tmp.order]
        }
      }
      pred <- NA
      if(predictions){
        g0 = feval(models$fg_pointer,x_i,a_i,parameters$bpop[,2,drop=F],zeros(1,d_size),zeros(docc_size,NumOcc))
        
        pred <- feval(models$ff_pointer,model_switch_i,xt_i,g0,poped.db)
        pred <- drop(pred[[1]])
      }    
      group.df <- data.frame(Time=xt_i,PRED=pred,Group=groups_to_use[i],Model=model_switch_i)
      #     group.df <- data.frame(Time=xt_i,PRED=drop(pred[[1]]),Group=groups_to_use[i],
      #                            ##paste("Group",i,sep="_"),
      #                            Model=model_switch_i)
      #     
      
      if(include_a && !isempty(a_i)) group.df <- data.frame(group.df,a_i)
      if(include_x && !isempty(x_i)) group.df <- data.frame(group.df,x_i)
      
      
      
      #     if(include_id){
      #       #parameters$groupsize[groups_to_use[i]]
      #       id_num_end <- id_num_start + num_ids - 1
      #       group.df <- merge(group.df,data.frame(ID=id_num_start:id_num_end))
      #       id_num_start <- id_num_end + 1 
      #       group.df <- group.df[c(length(group.df),1:(length(group.df)-1))] #reorder columns
      #     }     
      
      if(IPRED){
        group.df.ipred <- data.frame()
        bocc_start= 1
        id_num_end <- id_num_start + num_ids - 1
        id_vals <- id_num_start:id_num_end
        
        if(predictions){
          fulld = getfulld(parameters$d[,2],parameters$covd)
          fulldocc = getfulld(parameters$docc[,2,drop=F],parameters$covdocc)
          b_sim_matrix = rmvnorm(num_ids,sigma=fulld)
          bocc_sim_matrix = zeros(num_ids*NumOcc,length(parameters$docc[,2,drop=F]))
          if(nrow(fulldocc)!=0) bocc_sim_matrix = rmvnorm(num_ids*NumOcc,sigma=fulldocc)
        }
        
        for(j in 1:num_ids){
          tmp.df <- group.df
          if(predictions){
            bocc_stop=bocc_start + NumOcc - 1
            if(nrow(fulldocc)==0){ 
              bocc_start=0
              bocc_stop=0
            }
            fg_sim = feval(models$fg_pointer,x_i,a_i,parameters$bpop[,2,drop=F],b_sim_matrix[j,],t(bocc_sim_matrix[bocc_start:bocc_stop,]))
            bocc_start = bocc_stop + 1
            ipred <- feval(models$ff_pointer,model_switch_i,xt_i,fg_sim,poped.db)
            ipred <- drop(ipred[[1]])
          } else {  
            ipred <- xt_i*NA
          }
          #ID <- (i-1)*num_ids+j
          ID <- id_vals[j] 
          tmp.df["ID"] <- ID
          tmp.df["IPRED"] <- ipred
          
          if(DV){
            if(predictions){
              eps_sim = rmvnorm(length(xt_i),sigma=parameters$sigma)
              dv <- feval(models$ferror_pointer,model_switch_i,xt_i,fg_sim,eps_sim,poped.db) 
              dv <- drop(dv[[1]])
            } else {
              dv <- xt_i*NA
            }
            tmp.df["DV"] <- dv
          }
          
          if(!is.null(dosing)){
            dose.df <- data.frame(dosing[groups_to_use[i]])
            for(nam in names(tmp.df[!(names(tmp.df) %in% c("IPRED","PRED","DV","Time"))])){
              #if(length(unique(tmp.df[nam]))==1) dose.df <- data.frame(dose.df,nam=tmp.df[1,nam])
              if(length(unique(tmp.df[nam]))==1) dose.df[nam] <- tmp.df[1,nam]  
            }
            dose.df$dose_record_tmp <- 1
            tmp.df <- dplyr::rbind_list(dose.df,tmp.df)
            tmp.df <- tmp.df[order(tmp.df$Time,tmp.df$dose_record_tmp),]
            tmp.df$dose_record_tmp <- NULL
          }
          
          
          
          group.df.ipred <- rbind(group.df.ipred,tmp.df)
        }
        id_num_start <- id_num_end + 1 
        group.df <- group.df.ipred
      }
      
      df <- rbind(df,group.df)
      #model.pred  <- rbind(model.pred,i=returnArgs[[1]])
      #model.pred[paste("Group",i,sep="_")]  <- returnArgs[[1]]
      
    }
    
    #reorder columns
    first_names <- c("ID","Time","DV","IPRED","PRED")
    first_names <- first_names[first_names %in% names(df)] 
    other_names <- names(df[!(names(df) %in% first_names)])
    df <- df[c(first_names,other_names)]
    
    df$Group <- as.factor(df$Group)
    df$Model <- as.factor(df$Model)
    if(IPRED) df$ID <- as.factor(df$ID)
    
    row.names(df) <- NULL
    
    if(!is.null(filename)) write.table(x=df, filename, row.names=FALSE, quote=FALSE, na=".",sep=",") 
    
    return( df ) 
  }) # end with(design,{})
}