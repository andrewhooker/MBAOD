print_design <- function (design,file="") {
  with(design,{
    if(exists("xt")) print_xt(xt,ni,model_switch,fn=file,head_txt="\nSampling Schedule -----\n\n")
    if(exists("a")){
      cat("\nCovariates -----\n",file=file)
      rownames(a) <- gsub("grp_","Group ",rownames(a))
      rownames(a) <- paste(rownames(a)," : ", sep="")
      #print(a,digits=3)
      cat(capture.output(print(a,digits=3)),sep="\n",file=file)
    }
    if(exists("x")){
      cat("\nDiscrete Variables -----\n",file=file)
      rownames(x) <- gsub("grp_","Group ",rownames(x))
      rownames(x) <- paste(rownames(x)," : ", sep="")
      #print(x,digits=3)
      cat(capture.output(print(x,digits=3)),sep="\n",file=file)
    }
    if(exists("groupsize")){
      cat("\nGroupsize -----\n",file=file)
      rownames(groupsize) <- gsub("grp_","Group ",rownames(groupsize))
      rownames(groupsize) <- paste(rownames(groupsize)," : ", sep="")
      colnames(groupsize) <- ""
      #print(groupsize,digits=3)
      cat(capture.output(print(groupsize,digits=3)),sep="\n",file=file)
    }
  })
}