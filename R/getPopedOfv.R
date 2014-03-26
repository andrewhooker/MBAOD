

getPopedOfv <- function(){  
  
  print('- Extract Poped final OFV/FIM')
  
  o <- file("output.log")
  out <- readLines(o)
  close(o)
  
  lineOFV <- grep("ofvmf:", out)
  ofv <- as.numeric(unlist(strsplit(out[lineOFV], ":"))[2])
  
  return(ofv)    
}


getPopedOutputFile <- function(){  
  
  print('- Extract Poped output file name')
  
  o <- file("output.log")
  out <- readLines(o)
  close(o)
  
  lineOUT <- grep("Result of optimization in file:", out)
  strsplit(out[lineOUT], ": ")
  file.name <- unlist(strsplit(out[lineOUT], ": "))[2]
  
  return(file.name)    
}


