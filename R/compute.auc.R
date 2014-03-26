

compute.auc <- function(final.par.list){
  AUC <- c()
  for(j in 1:dim(final.par.list)[1]){
    for(i in 1:70){
      ##j=1
      ##i=1
      final.par <- final.par.list[j,]
      wt=i
      dose=1000#*(wt/70)
      trueAUC=dose/(1+ (2 * wt^5)/ (25^5 + wt^5))
      obsAUC=dose/(final.par$thCl+ (final.par$thMax * wt^final.par$thHill)/ (final.par$thE50^final.par$thHill + wt^final.par$thHill))
      AUC <- rbind(AUC,c(iter=j,wt=wt,dose=dose,trueAUC=trueAUC,obsAUC=obsAUC))
    }
  }
  AUC.2 <- as.data.frame(AUC)
  return(AUC.2)
}