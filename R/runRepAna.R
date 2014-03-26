runRepAna <- function(name,
                      rep,                      
                      nsteps,
                      prev,
                      models,
                      design.files,
                      cur.cov,
                      unfix,
                      cur.groupsize,
                      samples,
                      settings,
                      mpar,
                      fixed,
                      description,
                      overwrite=FALSE,
                      ...)
{
  # check if file exists
  if(file.exists(name)){
    if(overwrite){
      cat(paste("Deleting directory ",name))
      unlink(name, recursive=T)
    } else {
      stop("Directory ",name, " exists")
    }
  } 
  
  ## copy needed files to directory
  dir.create(name)
  file.copy(c(unlist(models)), name, overwrite=TRUE) # copy model files
  file.copy(c(unlist(design.files)), name, overwrite=TRUE) # copy design files
  file.copy(c(unlist(settings$poped.sh.script)), name, overwrite=TRUE) # copy shell scripts
  
  setwd(paste("./",name,sep=""))  
  
  models_names <- lapply(models,basename)
  design_file_names <- lapply(design.files,basename) 
  settings$poped.sh.script <- basename(settings$poped.sh.script)

  resall<-list()
  for(i in 1:rep){
    print('')
    print('----------------------')
    print(paste('--------- Running Iteration', i))
    print('----------------------')
    print('')
    
    res<-runAdaptiveDesignStep(nsteps,
                               prev,
                               models_names,
                               cur.cov,
                               unfix,
                               cur.groupsize,
                               samples,
                               settings, # detFIM or Ds
                               mpar,
                               fixed,# initial mat. parameters
                               name=name)
    res$description<-description
    resall[[i]]<-res
    
  }
  
  save(x=resall, file=paste("../resultI_", name, ".Rdata", sep=""))
  
  setwd("..")
  
}
