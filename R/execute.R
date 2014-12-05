execute <- function(models,
                    remote=FALSE,
                    removefolder=FALSE, 
                    ssh_command = NULL, #"ssh andy@doris.biof.uu.se 'cd AOD/",
                    additional_commands=NULL,
                    run_dir=NULL,
                    ...)
{
  
  if(Sys.info()['sysname']!='Windows'){
    if(!is.null(ssh_command)) ssh_command <- paste(ssh_command,"; ",sep="")
    cd_command <- NULL
    if(!is.null(run_dir)) cd_command <- paste("cd ",run_dir,"; ",sep="")
    command <-paste(ssh_command, cd_command, "execute ", paste(models,collapse=" ")," ",additional_commands,sep="")
    
    if(Sys.which("execute")=="") command <- paste("source ~/.bashrc;",command,sep="")
    
    system(command)
  }else{
    shell(sprintf("execute %s", paste(paste0(sprintf("%s/",run_dir), models), collapse=" ")))
  }
  # cleanup folders
  if(removefolder){
    files <- dir(pattern="mod\\.dir")
    while(length(files!=0)){
      unlink(files, recursive=TRUE) # remove psn folder
      files <- dir(pattern="mod\\.dir")
    }
  }
  
}
