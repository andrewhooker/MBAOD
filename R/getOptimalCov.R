getOptimalCov <- function(file.name="1.m"){
  
  print('- Extract optimal covariates')
  
  o <- file(file.name)
  out <- readLines(o, warn=FALSE)
  close(o)
  
  # covariates
  lineCov <- grep('popedOutput.a', out)
  optCov  <- out[lineCov]
  optCov  <- gsub("\\]",";",optCov)[[1]]
  optCov  <- gsub("\\[",";",optCov)[[1]]
  optCov  <- strsplit(optCov, c(";"))[[1]]
  optCov  <- optCov[!optCov %in% c("popedOutput.a=", "", "'", " ")]
  optCov  <- strsplit(optCov, ",")[[1]]
  cov=list()
  for(i in 1:length(optCov)){
    cov[[i]] <- as.numeric(optCov[i])
  }
  
  # sampling times
  lineTimes <- grep('popedOutput.xt', out)
  optTimes  <- paste(out[lineTimes:(lineCov-5)], collapse=" ")
  optTimes  <- gsub("\\]",";",optTimes)[[1]]
  optTimes  <- gsub("\\[",";",optTimes)[[1]]
  optTimes  <- strsplit(optTimes, ";")[[1]]
  optTimes  <- optTimes[!optTimes %in% c("popedOutput.xt=", "", " ")]
  times<-list()
  for(i in 1:length(optTimes)){
    timesi <- strsplit(optTimes[i], " ")[[1]]
    timesi <- timesi[!timesi %in% ""]
    times[[i]] <- as.numeric(timesi)
  }
  
  return(list(optTimes=times, optCov=cov))
}