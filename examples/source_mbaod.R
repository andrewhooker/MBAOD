source_mbaod <- function (...) {
  # source the files in this package
  sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
    }
  }
  
  sourceDir("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/R",trace=FALSE)
}