

runPoped_matlab <- function(remote=remote, name=name, sh.script="run.sh",cluster=T){  
  print('- Running POPED')
  
  # remove old output.log
  unlink("output.log")
  
  # run optimization / call PopED
  #com="/usr/local/MATLAB/R2011b/bin/matlab -nosplash -nodisplay -nodesktop -r \"poped(function_input()); quit\" -logfile output.log"
  if(remote == TRUE) com <- paste("ssh andy@doris.biof.uu.se 'cd AOD/coen/",name,"; bash ",sh.script,"'", sep="")  
  if(remote == FALSE) com <- paste("bash ", sh.script, sep="")
  
  if(Sys.which("execute")=="") com <- paste("source ~/.bashrc;",com,sep="")
  
  print(com);
  
  # run poped
  if(!cluster){
    system(com)
  } else {
    system(com, wait=TRUE)
    # check if run is already finished
    run <- TRUE
    cat("Running")
    while(run == TRUE){
      
      #if(remote == TRUE) state <- system("ssh doris.biof.uu.se qstat", intern=TRUE)
      #if(remote == FALSE) state <- system("qstat", intern=TRUE)
      
      #if(length(state) > 0) { Sys.sleep(4)}
      #if(length(state) == 0) { print('finished poped'); run=FALSE}
      
      # read output file
      
      if(file.exists("output.log")){
        o<-file("output.log")
        f<-readLines(o)
        close(o)
        
        finished <- "              strengine: 'MATLAB 7.9.0.529 (R2009b)'"
        tail <- tail(f,2)[1]
        if( !is.na(tail) ) {
          if( is.logical(tail == finished)  ){
            if( tail == finished  ){
              print('Finished poped');
              run=FALSE;
            } else {
              cat('.'); Sys.sleep(3)
            }
          } else  {
            cat('.'); Sys.sleep(3)
          }
        } else {
          cat('.'); Sys.sleep(3)
        }    
      } 
    }
  }
}
