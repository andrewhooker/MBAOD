mbaod_simulate <- function(cohorts,
                           ncohorts=length(cohorts),
                           rep=1,      
                           name = "MBAOD_run",
                           description = NULL,
                           zip_directories=T,
                           sim_data_input_fn="sim_data.csv",
                           sim_data_output_fn="mc_sim_1.tab",
                           sim_data_manip_fun = NULL,
                           seednr=NULL,
                           ED_uncertianty = 0.1,
                           ED_only_FIM = F,
                           update_params =T,
                           stop_crit_fun = NULL,
                           optim_fun ="poped_optim",
                           run_commands = "",...){
  
  # timing
  tic(name=".mbaod_start_time")
  if(!is.null(seednr)) set.seed(seednr)
  pars_out <- NULL
  # check if directory exists
  valid_name  <- FALSE
  i <- 0
  dir_names_all <- base::list.dirs(recursive = FALSE)
  dir_names <- grep(paste(name,"_run_dir_(\\d+)",sep=""),dir_names_all,value=T)
  if(length(dir_names)>0) i <- as.numeric(max(gsub(paste(".*",name,"_run_dir_(\\d+)",sep=""),"\\1",dir_names)))
  while(!valid_name){
    i <- i+1
    run_dir <- paste(name,"_run_dir_",i,sep="")
    valid_name <- !file.exists(run_dir)
  }
  dir.create(run_dir)
  
  
  
  # create the extra cohorts if needed
  if(length(cohorts)<ncohorts) cohorts[(length(cohorts)+1):ncohorts] <- cohorts[length(cohorts)]
  
  ## copy needed files to directory
  #file.copy(c(unlist(models)), name, overwrite=TRUE) # copy model files
  #if(!is.null(design.files)) file.copy(c(unlist(design.files)), name, overwrite=TRUE) # copy design files
  #file.copy(c(unlist(settings$poped.sh.script)), name, overwrite=TRUE) # copy shell scripts
  
  #setwd(paste("./",name,sep=""))  
  
  #models_names <- lapply(models,basename)
  #design_file_names <- lapply(design.files,basename)
  #settings$poped.sh.script <- basename(settings$poped.sh.script)
  
  for(j in 1:rep){
    #Adds the list to store all output from stopping function if it is present
    if(!is.null(stop_crit_fun)) stop_res <- list()
    
    cat('\n')
    cat('----------------------\n')
    cat(paste('--------- Running Iteration', j,"\n"))
    cat('----------------------\n')
    cat('\n')
    
    rep_dir <- file.path(run_dir,paste("rep_",j,sep=""))
    dir.create(rep_dir)
    
    aod_res <- list()
    
    # Cleanup   
    #cleanup()
    
    # Copy model files to default names               
    #copyModels(models)
    
    # output file names from NONMEM
    #     est.mods <- c(reduced=models$modredest,full=models$modfullest)
    #     out.lst <- gsub("(.*)(\\.[^\\.]*)$","\\1",est.mods)
    #     out.lst <- paste(out.lst,".lst",sep="")
    #     names(out.lst) <- names(est.mods)
    
    # Simulate one MBAOD experiment
    for(i in 1:ncohorts){
      cat('\n')
      cat('----------------------\n')
      cat('--------- Running cohort', i,"in iteration",j,"\n")
      cat('----------------------\n')
      cat('\n')
      
      cohort_dir <- file.path(rep_dir,paste("cohort_",i,sep=""))
      dir.create(cohort_dir)
      
      cohort_res <- list()
      
      cohort <- cohorts[[i]]
      
      
      
      #----------  optimize the cohort --------
      if(!is.null(cohort$optimize)){
        
        cat('\n')
        cat('    ----------------------\n')
        cat('    --------- Optimizing design for Cohort',i,"in iteration",j,"\n")
        cat('    ----------------------\n')
        cat('\n')
        
        if(cohort$optimize$target=="poped_R"){
          
          #           cohort <- step_2
          #           load("Example_1_run_dir_4/resultI_Example_1.Rdata")
          #           aod_res <- results_all[[1]]
          #           cohort_dir="."
          
          #### combine initial design for current and  final design for all previous cohorts ----------
          design_and_space <- combine_design_and_space(cohort, aod_res)
          tot_design <- design_and_space$design
          tot_space <- design_and_space$design_space
          
          #### Parameter estimates ------------------
          # results from previous cohort + new parameters if needed
          prev_res <- NULL
          if(!length(aod_res)==0) prev_res <- aod_res[[length(aod_res)]]
          
          if(!is.null(prev_res) & update_params==T) {
            parameters <- merge_parameters(prev_res$est_result,cohort$optimize$parameters)
          }else{
            parameters <- cohort$optimize$parameters
          }
          
          if(!is.null(cohort$optimize$settings.opt$d_switch)){
            if(!cohort$optimize$settings.opt$d_switch){
              bpop_vals_ed <- cbind(zeros(length(parameters$bpop),1),parameters$bpop,zeros(length(parameters$bpop),1)) 
              bpop_vals_ed[,1] <- cohort$optimize$settings.db$notfixed_bpop
              if(i==1){
                bpop_vals_ed[,3] <- (cohort$optimize$settings.db$notfixed_bpop*rep(ED_uncertianty,length(parameters$bpop))*parameters$bpop)^2
              }else{
                if(ED_only_FIM==F){
                  theta_rows <- c(paste("THETA",1:length(parameters$bpop),sep=""))
                  covmat <- cov_mat[rownames(cov_mat) %in% theta_rows,colnames(cov_mat) %in% theta_rows]
                  covmat <- as.matrix(covmat)
                  bpop_vals_ed[,3] <- diag(covmat)
                }else{
                  print("Updating Priors based on FIM")
                  nf <- cohort$optimize$settings.db$notfixed_bpop
                  bpop_vals_ed[,3]<-c(nf[nf!=1],1/diag(aod_res[[paste("cohort_",i-1,sep="")]]$opt_result$opt_output$fmf)[1:sum(nf)])
                  
                }
              }
              parameters$bpop <- bpop_vals_ed
              
            } 
          }
          
          #### Create design database --------------
          # add prior FIM here if prev_fim=T
          design.db <- create_design_database(tot_design, tot_space, parameters, cohort)
          
          #### optimize cohort ------------
          opt_output  <- do.call(optim_fun,
                                 c(poped.db=list(design.db),
                                   cohort$optimize$settings.opt,
                                   out_file=file.path(cohort_dir,"PopED_ouput.txt")))
          
          # update total design with optimized design from output          
          optimized_design <- update_design(tot_design,opt_output)
          
          # extract only cohort's optimized design
          optimized_design_cohort <- extract_cohort_design(optimized_design,cohort)          
          
          # store optimization results in cohort results
          cohort_res$opt_result$opt_design_full <- optimized_design
          cohort_res$opt_result$opt_design_cohort <- optimized_design_cohort
          cohort_res$opt_result$opt_output <- opt_output
          
          # pass to other steps in AOD
          cohort$design <- optimized_design_cohort
          
        } # end poped
      }
      
      #----- simulate  the  cohort ---------
      if(!is.null(cohort$simulate)){
        
        cat('\n')
        cat('    ----------------------\n')
        cat('    --------- Simulating Data for Cohort',i,"in iteration",j,"\n")
        cat('    ----------------------\n')
        cat('\n')
        
        if(cohort$simulate$target=="poped_R"){
          ## create data set with simulated values
          poped.db <- do.call(create.poped.database,
                              c(do.call(create_design,cohort$design),
                                cohort$simulate$model,
                                cohort$simulate$parameters))
          
          sim_data <- model_prediction(poped.db=poped.db,
                                       DV=T,
                                       dosing=cohort$simulate$data$dosing,
                                       filename=file.path(cohort_dir,sim_data_output_fn),
                                       manipulation=cohort$simulate$data$manipulation)
          
        }
        
        if(cohort$simulate$target=="NONMEM"){
          ## create data set
          sim_data <- model_prediction(DV=T,
                                       design=cohort$design,
                                       dosing=cohort$simulate$data$dosing,
                                       #filename=file.path(cohort_dir,sim_data_input_fn),
                                       manipulation=cohort$simulate$data$manipulation)
          
          if(!is.null(sim_data_manip_fun)) sim_data <- sim_data_manip_fun(sim_data,...)
          write.table(sim_data, file.path(cohort_dir,sim_data_input_fn), row.names=FALSE, quote=FALSE, na=".",sep=",") 
          
          ## copy simulation model to directory with name sim_orig.mod
          file.copy(cohort$simulate$model, file.path(cohort_dir,"sim_orig.mod")) 
          
          ## change the seed number in the file
          change_seed_number(file.path(cohort_dir,"sim_orig.mod"),
                             file.path(cohort_dir,"sim.mod"))
          
          ## for simulation model
          ## change $DATA to right file name (sim.data.csv)
          ## change $INPUT so that it matches sim_data, match with grep, if no match then throw an error, add drop to columns not needed
          ## add table output so that you get same as $INPUT
          print(cohort_dir)
          execute("sim.mod",run_dir=cohort_dir,...)
        }
        
        
        cohort_res$design <- cohort$design
        cohort_res$dosing <- cohort$simulate$dosing
        
        
      }
      
      if(!is.null(cohort$estimate)){
        
        cat('\n')
        cat('    ----------------------\n')
        cat('    --------- Estimating parameters for Cohort',i,"in iteration",j,"\n")
        cat('    ----------------------\n')
        cat('\n')
        
        if(cohort$estimate$target=="NONMEM"){
          
          est_successful <- 0
          
          ## copy estimation model to directory with name est.mod
          file.copy(cohort$estimate$model, file.path(cohort_dir,"est.mod")) 
          
          ## merge simulated data from this cohort and other cohorts
          file.create(file.path(cohort_dir,"est.dat"))
          data_vec <- file.path(rep_dir,paste("cohort_",1:i,sep=""),sim_data_output_fn)
          file.append(file.path(cohort_dir,"est.dat"),data_vec)
          
          ## change $DATA to match sim data name with simulated data " mc_sim_1.tab"
          ## change $INPUT so that it matches  mc_sim_1.tab, match with grep, if no match then throw an error, add drop to columns not needed
          
          #test for successful run, retry if unsuccessful
          ## output result to screen
          ## get all estimation info from xpose
          execute("est.mod",run_dir=cohort_dir,additional_commands=run_commands,...)
          est_result <- run_results(cohort_dir)
          cohort_res$est_result <- est_result
          
          
          if(is.null(est_result)){
            print("NONMEM estimation failed ...")
            pars <- NA
            cov_mat <- NA
          }else{
            
            ## get params and RSE from xpose
            pars <- getPars(file.path(cohort_dir,"est.lst"))
            
            ## get cov matrix
            cov_mat <- read_covmat(cohort_dir)
            cohort_res$est_result$cov_mat <- cov_mat
            
          }
        }##end nonmem est
      }
      
      ####STOPPING CRITERIA####
      if(!is.null(stop_crit_fun)){
        stop_res_tmp <- stop_crit_fun(i,cohort_res,design.db=design.db, opt_setting = cohort$optimize$settings.opt,cohort_dir = cohort_dir)     #returns list with stop_MBAOD = T/F and new x.space
        aod_res[[paste("cohort_",i,sep="")]] <- cohort_res
        stop_res[[paste("cohort_",i,sep="")]] <- stop_res_tmp
        if(stop_res_tmp[1]==TRUE) break
      }else{
        aod_res[[paste("cohort_",i,sep="")]] <- cohort_res
      }
      
      ####END STOPPING CRITERIA#####
    } # end cohort
    
    ## summarize results for one iteration
    cat('\n')
    cat('----------------------\n')
    cat('--------- Summary of results for iteration ',j,"\n")
    cat('----------------------\n')
    cat('\n')
    
    toc(echo=T,name=".mbaod_start_time")
    cat("\n")
    
    cat('--------- Final Design\n')
    final_design <- combine_designs(aod_res)
    print_design(final_design)
    
    cat('\n--------- Parameter estimation after each cohort\n')
    #aod_res
    #aod_res$cohort_1$est_result
    
    #xpose4::read.lst(file.path(cohort_dir,"est.lst"))
    #xpose4::create.parameter.list(file.path(cohort_dir,"est.lst"))
    #getPars(file.path(cohort_dir,"est.lst"))
    #Hmisc::print.char.matrix(getPars(file.path(cohort_dir,"est.lst")),col.names=T)
    est_summary <- summarize_estimation(aod_res)
    est_summary$iter <- j
    #print(est_summary,digits=3)
    #sumo("est.lst",run_dir=cohort_dir,...)
    
    aod_res$est_summary <- est_summary
    aod_res$final_design <- final_design
    
    #Adds all the stoppign functions for this results to the aod res.
    if(!is.null(stop_crit_fun)) aod_res$stop_res <- stop_res
    
    save(aod_res, file=file.path(run_dir,paste("results_rep_",j,".Rdata",sep="")))
    
    # zip iteration directory
    if(zip_directories){
      #system("cd Example_1_run_dir_12 ; zip -rq rep_1.zip rep_1")
      #WD <- getwd()
      #setwd(run_dir)
      zip(paste(rep_dir,".zip",sep=""),rep_dir,flags="-r9Xq")
      unlink(rep_dir,recursive = T)
    }
    pars_out <- rbind(pars_out, as.numeric(pars[,2]))
    
    
  } # end rep
  
  results_all<-list()
  est_summary_total <- c()
  for(file_name in grep("results_rep_",list.files(run_dir,full.names=T),value=T)){
    load(file_name)
    est_summary <- aod_res$est_summary
    est_summary_total <- rbind(est_summary_total,est_summary)
    iter <- sub(".*results_rep_(\\d*).*","\\1",file_name)
    results_all[[paste("iteration_",iter,sep="")]] <- aod_res
  }
  results_all$description<-description
  results_all$name<-name
  results_all$est_summary_total <- est_summary_total
  results_all$pars_out <- pars_out
  #pars_out <- matrix(pars_out, ncol=length(pars))
  save(results_all, file=file.path(run_dir,paste("results_all", ".Rdata",sep="")))
  
  return(results_all)
}

read_covmat<- function(dir,name="est.cov"){
  file_name <- file.path(dir,"est.cov")
  if(!file.exists(file_name)){
    message("Covariance matrix ", name," does not exist at ", dir)
    return(NULL)
  }
  tryCatch({
    read.table(file_name,skip=1,header = T,row.names=1,check.names=T)
  }, error=function(cond){
    message("Failed to read Covariance Matrix")
    return(NULL)
  }
  )
}

run_results <- function(cohort_dir){
  out = tryCatch ({
    sumo("est.lst",run_dir=cohort_dir)
    est_result <- xpose4::read.lst(file.path(cohort_dir,"est.lst"))
  },
  
  error = function(cond){
    message("The est.lst file was not found.")
    message("Original error message:")
    message(cond)
    message("")
    return("NULL")
  } 
  )
  return(out)
}