runSSE <- function(prev, step=1, simple=FALSE, remote=TRUE,
                   removefolder=TRUE, name=name,
                   ssh.command = "ssh andy@doris.biof.uu.se 'cd AOD/coen/",
                   clean=3,
                   run.on.sge=T,
                   models)
{
  print('- Running SSE')
  
  #   delete prior COI and LST files
  #   if(file.exists('runfE.lst')) unlink('runfE.lst')
  #   if(file.exists('runrE.lst')) unlink('runrE.lst')
  #   if(file.exists('runfE.coi')) unlink('runfE.coi')
  #   if(file.exists('runrE.coi')) unlink('runrE.coi')
  
  if(run.on.sge){sge.text=" -run_on_sge "}else{sge.text=""}
  
  # run model - simulation allways with the full model
  if(remote == TRUE){    
    #if(simple == FALSE) command <-paste(ssh.command,name,"; execute runfS.mod -sge_queue=all.q@n27 -clean=",clean," -run_on_sge -directory='sim' '",sep="")
    #if(simple == TRUE)  command <-paste(ssh.command,name,"; execute runrS.mod -sge_queue=all.q@n27 -clean=",clean," -run_on_sge -directory='sim'  '",sep="")
    command <-paste(ssh.command,name,"; execute ", models$modfullsim," -sge_queue=all.q@n27 -clean=",clean, sge.text,sep="")
  }
  
  if(remote == FALSE){    
    #if(simple == FALSE) command <-"execute runfS.mod -clean=3 -run_on_sge -directory='sim'  "
    #if(simple == TRUE)  command <-"execute runrS.mod -clean=3 -run_on_sge -directory='sim' "
    #if(simple == FALSE) command <-paste("execute runfS.mod -clean=",clean," -run_on_sge -directory='sim'  ",sep="")
    #if(simple == TRUE)  command <-paste("execute runrS.mod -clean=",clean," -run_on_sge -directory='sim'  ",sep="")
    command <-paste("execute ", models$modfullsim, " -clean=",clean, sge.text, sep="")
  }  
  
  if(Sys.which("execute")=="") command <- paste("source ~/.bashrc;",command,sep="")
  system(command) 
  
  # if previous experiment has been conducted add these simulations to current simulation
  if(length(prev[[step]]$dataset) > 0){  
    
    print(' adding previous')
    
    currsim <- read.table("outA.tab")
    names(currsim) <- c("id","time","dv","amt","wt")
    names(prev[[step]]$dataset) <- c("id","time","dv","amt","wt")    
    all <- rbind(currsim, prev[[step]]$dataset)      
    
    write.table(x=all, quote=FALSE, file="outA.tab", row.names=FALSE, na=".")        
  }  
  
  # run model - estimation
  if(remote == TRUE){
    #if(simple == FALSE) command <- paste("ssh andy@doris.biof.uu.se 'cd AOD/coen/",name,"; execute runfE.mod -sge_queue=all.q@n27 -clean=3 -run_on_sge   '",sep="")
    #if(simple == TRUE)  command <- paste("ssh andy@doris.biof.uu.se 'cd AOD/coen/",name,"; execute runrE.mod -sge_queue=all.q@n27 -clean=3 -run_on_sge   '",sep="")  
    if(simple == FALSE) command <- paste(ssh.command,name,"; execute ",models$modfullest," -sge_queue=all.q@n27 -clean=",clean,sge.text,sep="")
    if(simple == TRUE)  command <- paste(ssh.command,name,"; execute ",models$modredest," -sge_queue=all.q@n27 -clean=",clean,sge.text,sep="")  
  }
  if(remote == FALSE){
    #if(simple == FALSE) command <- "execute runfE.mod -clean=3 -run_on_sge  "
    #if(simple == TRUE)  command <- "execute runrE.mod -clean=3 -run_on_sge  "  
    if(simple == FALSE) command <- paste("execute ", models$modfullest,sge.text,sep="")
    if(simple == TRUE) command <- paste("execute ", models$modredest,sge.text,sep="")
  }
  
  if(Sys.which("execute")=="") command <- paste("source ~/.bashrc;",command,sep="")
  system(command)
  
  # cleanup folders
  if(removefolder){
    files <- dir(pattern="mod\\.dir")
    while(length(files!=0)){
      unlink(files, recursive=TRUE) # remove psn folder
      files <- dir(pattern="mod\\.dir")
    }
  }
}
