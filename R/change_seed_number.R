change_seed_number <- function (infile,outfile) {
  o <- file(infile)
  f <- readLines(o)
  close(o)                    
  f_new <- f
  #f_new  <- c(f_new,"")
  sim_lines <- grep("\\$SIM",f_new)          
  new_str <- f_new[sim_lines]
  splt_str <- stringr::str_split(new_str,"\\)")
  for(ii in 1:length(splt_str)){
    sample <- sample(1:10000000,length(splt_str[[ii]]))
    splt_str_tmp <- stringr::str_replace_all(splt_str[[ii]],"(\\(\\s*)\\d*(.*)",
                                             paste("\\1",sample,"\\2",sep=""))
    new_str[[ii]] <- paste(splt_str_tmp,collapse=")")
  }          
  f_new[sim_lines] <- new_str
  writeLines(f_new,outfile)
  return(invisible(f_new))
}