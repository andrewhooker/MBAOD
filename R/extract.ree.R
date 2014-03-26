
extract.ree <- function(summary) {  
  #summary[[2]][1:20,1:10]
  
  #!duplicated(summary[[2]]$rep,fromLast=T)
  #foo<-summary[[2]][!duplicated(summary[[2]]$rep,fromLast=T),]
  #foo[1:20,1:10]
  
  #final.par <- subset(summary[[2]],grp==4)
  #browser()
  final.par <- summary[[2]][!duplicated(summary[[2]]$rep,fromLast=T),]
  cat("Number of parameter sets:",dim(final.par)[1])
  final.par.ree <- final.par
  final.par.ree$thCl <- (final.par$thCl - 1)/1 * 100
  final.par.ree$thV <- (final.par$thV - 20)/20 * 100
  final.par.ree$thMax <- (final.par$thMax - 2)/2 * 100
  final.par.ree$thE50 <- (final.par$thE50 - 25)/25 * 100
  final.par.ree$thHill <- (final.par$thHill - 5)/5 * 100  
  final.par.ree$omCL <- (final.par$omCL - 0.05)/0.05 * 100
  final.par.ree$omV <- (final.par$omV - 0.05)/0.05 * 100
  final.par.ree$sigP <- (final.par$sigP - 0.015)/0.015 * 100
  final.par.ree$sigA <- (final.par$sigA - 0.0015)/0.0015 * 100
  return(final.par.ree)
}

