cleanup <- function(){
  
  print('- Cleaning up..')
  
  # remove old files before new run
  unlink(dir(pattern="runA")[!dir(pattern="runA") %in% dir(pattern="runA")[grep("mod", dir(pattern="runA"))]])
  unlink(dir(pattern="runB")[!dir(pattern="runB") %in% dir(pattern="runB")[grep("mod", dir(pattern="runB"))]])
  
  files<-c("xml","_L","_R", "INTER", "LINK", "OUTPUT", "nul",
           "nmprd4p.mod", "nonmem",
           "nmfe72", "set", "newline", "gfortran", "prsizes",
           "trash", "compile", "matlab",
           "garbage.out", paste(1:30, ".m", sep=""))
  
  for(f in files){    
    unlink(dir(pattern=f))
  }
  
  # remove PsN folders
  if( length(dir(pattern="modelfit"))>0){
    system('rm -r modelfit*')
  }
  
}
