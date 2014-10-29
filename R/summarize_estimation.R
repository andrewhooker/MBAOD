summarize_estimation <- function (result_list,file="",omega_sd=T,sigma_sd=T,RSE=T,digits=3) {
  results <- c()
  for(i in 1:length(result_list)){
    est_result <- result_list[[i]][["est_result"]]
    
    thetas <- rbind(est_result$thetas)
    names(thetas) <- paste("THETA_",1:(size(thetas,2)),sep="")    
    
    omegas_list <- est_result$omega
    omega <- matrix_from_triangle(omegas_list)
    omega_names <- omega*NA
    for(ii in 1:size(omega,1)){
      for(jj in 1:size(omega,2)){
        omega_names[ii,jj] <- paste("OMEGA_var",ii,jj,sep="_") 
      }
    }
    names(omega) <- omega_names
    omega_vec <- omega[lower.tri(omega,diag=TRUE)]
    
    # if compute omega_sd and correlation
    omega_cor_vec <- NULL
    if(omega_sd){
      omega_cor <- cov2cor(omega)
      diag(omega_cor) <- sqrt(diag(omega))
      omega_cor_names <- omega_cor*NA
      for(ii in 1:size(omega_cor,1)){
        for(jj in 1:size(omega_cor,2)){
          omega_cor_names[ii,jj] <- paste("OMEGA_cor",ii,jj,sep="_") 
          if(ii==jj) omega_cor_names[ii,jj] <- paste("OMEGA_sd",ii,jj,sep="_") 
        }
      }
      names(omega_cor) <- omega_cor_names
      omega_cor_vec <- omega_cor[lower.tri(omega_cor,diag=TRUE)]
    }
    
    sigmas_list <- est_result$sigma
    sigma <- matrix_from_triangle(sigmas_list)
    sigma_names <- sigma*NA
    for(ii in 1:size(sigma,1)){
      for(jj in 1:size(sigma,2)){
        sigma_names[ii,jj] <- paste("SIGMA_var",ii,jj,sep="_") 
      }
    }
    names(sigma) <- sigma_names
    sigma_vec <- sigma[lower.tri(sigma,diag=TRUE)]
    
    # if compute sigma_sd and correlation
    sigma_cor_vec <- NULL
    if(sigma_sd){
      sigma_cor <- cov2cor(sigma)
      diag(sigma_cor) <- sqrt(diag(sigma))
      sigma_cor_names <- sigma_cor*NA
      for(ii in 1:size(sigma_cor,1)){
        for(jj in 1:size(sigma_cor,2)){
          sigma_cor_names[ii,jj] <- paste("SIGMA_cor",ii,jj,sep="_") 
          if(ii==jj) sigma_cor_names[ii,jj] <- paste("SIGMA_sd",ii,jj,sep="_") 
        }
      }
      names(sigma_cor) <- sigma_cor_names
      sigma_cor_vec <- sigma_cor[lower.tri(sigma_cor,diag=TRUE)]
    }
    
    
    se_thetas <- rbind(est_result$sethetas)
    names(se_thetas) <- paste("SE_THETA_",1:(size(thetas,2)),sep="")    
    
    se_omegas_list <- est_result$seomegas
    se_omega <- matrix_from_triangle(se_omegas_list)
    se_omega_names <- se_omega*NA
    for(ii in 1:size(se_omega,1)){
      for(jj in 1:size(se_omega,2)){
        se_omega_names[ii,jj] <- paste("SE_OMEGA_var",ii,jj,sep="_") 
      }
    }
    names(se_omega) <- se_omega_names
    se_omega_vec <- se_omega[lower.tri(se_omega,diag=TRUE)]
    
    se_sigmas_list <- est_result$sesigmas
    se_sigma <- matrix_from_triangle(se_sigmas_list)
    se_sigma_names <- se_sigma*NA
    for(ii in 1:size(se_sigma,1)){
      for(jj in 1:size(se_sigma,2)){
        se_sigma_names[ii,jj] <- paste("SE_SIGMA_var",ii,jj,sep="_") 
      }
    }
    names(se_sigma) <- se_sigma_names
    se_sigma_vec <- se_sigma[lower.tri(se_sigma,diag=TRUE)]
    
    
    
    
    # remove fixed omegas and sigmas
    omega_vec_red <- omega_vec[omega_vec!=0]
    se_omega_vec_red <- se_omega_vec[omega_vec!=0]
    omega_cor_vec_red <- omega_cor_vec[omega_vec!=0]
    
    sigma_vec_red <- sigma_vec[sigma_vec!=0]
    se_sigma_vec_red <- se_sigma_vec[sigma_vec!=0]
    sigma_cor_vec_red <- sigma_cor_vec[sigma_vec!=0]
    
    # rse calculations
    rse_thetas <- NULL
    rse_omega_vec_red <- NULL
    rse_sigma_vec_red <- NULL
    rse_omega_vec_red_sd <- NULL
    rse_sigma_vec_red_sd <- NULL
    if(RSE){
      rse_thetas <- se_thetas/abs(thetas)
      names(rse_thetas) <- paste("RSE_THETA_",1:(size(thetas,2)),sep="") 
      
      rse_omega_vec_red <- se_omega_vec_red/abs(omega_vec_red)
      names(rse_omega_vec_red) <- gsub("SE_","RSE_",names(se_omega_vec_red))
      
      rse_sigma_vec_red <- se_sigma_vec_red/abs(sigma_vec_red)
      names(rse_sigma_vec_red) <- gsub("SE_","RSE_",names(se_sigma_vec_red))
      
      rse_omega_vec_red_sd <- rse_omega_vec_red/2
      names(rse_omega_vec_red_sd) <- gsub("_var_","_sd_",names(rse_omega_vec_red_sd))
      
      rse_sigma_vec_red_sd <- rse_sigma_vec_red/2
      names(rse_sigma_vec_red_sd) <- gsub("_var_","_sd_",names(rse_sigma_vec_red_sd))
    }
    
    min_succ=F
    if(length(grep("^MINIMIZATION SUCCESSFUL$",est_result$term)!=0)) min_succ=T
    
    results_cohort <- data.frame(as.matrix(t(c(cohort=i,
                                               thetas,
                                               omega_vec_red,
                                               sigma_vec_red,
                                               se_thetas,
                                               se_omega_vec_red,
                                               se_sigma_vec_red,
                                               omega_cor_vec_red,
                                               sigma_cor_vec_red,
                                               rse_thetas,
                                               rse_omega_vec_red,
                                               rse_sigma_vec_red,
                                               rse_omega_vec_red_sd,
                                               rse_sigma_vec_red_sd,
                                               OFV=est_result$ofv,
                                               Minimization_Successful=min_succ))))
    
    #results_cohort$cohort <- i #paste("Cohort",i,sep="_")
    
    results <- data.frame(dplyr::rbind_list(results,results_cohort))
  }
  
  results$cohort <- as.factor(results$cohort)
  results$Minimization_Successful <- as.factor(results$Minimization_Successful)  
  
  #reorder columns
  first_names <- c("cohort",
                   grep("^THETA",names(results),value=TRUE),
                   grep("^OMEGA",names(results),value=TRUE),
                   grep("^SIGMA",names(results),value=TRUE),
                   grep("^SE_THETA",names(results),value=TRUE),
                   grep("^SE_OMEGA",names(results),value=TRUE),
                   grep("^SE_SIGMA",names(results),value=TRUE),
                   grep("^RSE_THETA",names(results),value=TRUE),
                   grep("^RSE_OMEGA_var",names(results),value=TRUE),
                   grep("^RSE_SIGMA_var",names(results),value=TRUE),
                   grep("^RSE_OMEGA_sd",names(results),value=TRUE),
                   grep("^RSE_SIGMA_sd",names(results),value=TRUE))
  first_names <- first_names[first_names %in% names(results)] 
  other_names <- names(results[!(names(results) %in% first_names)])
  results <- results[c(first_names,other_names)]  
  
  # print matrix out
  out_mat <- format(results,digits=digits)
  
  ## filter results
  first_names <- c(grep("^THETA",names(results),value=TRUE))
  if(omega_sd){ 
    first_names <- c(first_names,grep("^OMEGA_sd|^OMEGA_cor",names(results),value=TRUE))
  } else {
    first_names <- c(first_names,grep("^OMEGA_var",names(results),value=TRUE))
  }
  if(sigma_sd){ 
    first_names <- c(first_names,grep("^SIGMA_sd|^SIGMA_cor",names(results),value=TRUE))
  } else {
    first_names <- c(first_names,grep("^SIGMA_var",names(results),value=TRUE))
  }
  if(RSE){
    first_names <- c(first_names,grep("^RSE_THETA",names(results),value=TRUE))
    if(omega_sd){ 
      first_names <- c(first_names,grep("^RSE_OMEGA_sd|^RSE_OMEGA_cor",names(results),value=TRUE))
    } else {
      first_names <- c(first_names,grep("^RSE_OMEGA_var",names(results),value=TRUE))
    }
    if(sigma_sd){ 
      first_names <- c(first_names,grep("^RSE_SIGMA_sd|^RSE_SIGMA_cor",names(results),value=TRUE))
    } else {
      first_names <- c(first_names,grep("^RSE_SIGMA_var",names(results),value=TRUE))
    }
  }
  out_mat <- out_mat[c(first_names,other_names)]
  
  out_mat$cohort <- NULL
  #out_mat <- t(out_mat)
  out_mat <- data.frame(t(out_mat))
  colnames(out_mat) <- paste("Cohort",results$cohort,sep="_")
  #out_mat[,] <- sprintf("%5.3g",out_mat[,])
  #print(out_mat,digits=3,zero.print = ".",quote=F,justify="right")
  
  
  cat(
    capture.output(
      print(out_mat,digits=3,
            zero.print = ".",
            quote=F,
            justify="right")),
    sep="\n",
    file=file)
  #print(Matrix::Matrix(t(results)),digits=3)
  #Hmisc::print.char.matrix(t(results),digits=3)
  
  
  
  return(invisible(results))
}

