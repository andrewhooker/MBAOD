sumo <- function(output_files,
                 ssh_command = NULL, #"ssh andy@doris.biof.uu.se 'cd AOD/",
                 run_dir=NULL,
                 additional_commands=NULL,
                 ...)
{
  if(Sys.info()['sysname']!='Windows'){
    if(!is.null(ssh_command)) ssh_command <- paste(ssh_command,"; ",sep="")
    cd_command <- NULL
    if(!is.null(run_dir)) cd_command <- paste("cd ",run_dir,"; ",sep="") 
    command <-paste(ssh_command, cd_command, "sumo ", paste(output_files,collapse=" ")," ",additional_commands,sep="")
    
    if(Sys.which("sumo")=="") command <- paste("source ~/.bashrc;",command,sep="")
    
    system(command) 
  }else{
    shell(sprintf("sumo %s", paste(paste0(sprintf("%s/",run_dir), output_files), collapse=" ")))
  }
  
}
