library(PopED)
source("check_design.R")

xt1 <- list(c(1,2,3),c(1,2,3,4))
xt4 <- list(c(1,2,3,4,5),c(1,2,3,4))
xt2 <- rbind(c(1,2,3,4),c(1,2,3,4))
xt3 <- c(1,2,3,4)

check_design(xt=xt1)
check_design(xt=xt4)
check_design(xt=xt2)
check_design(xt=xt3)

check_design(xt=xt3,m=3)
check_design(xt=xt1,m=3) # should give an error

check_design(xt=xt1,model_switch=ones(2,4))

check_design(xt=xt1,a=c(2,3,4))
check_design(xt=xt1,a=rbind(c(2,3,4),c(4,5,6)))
check_design(xt=xt1,a=list(c(2,3,4,6),c(4,5,6)))
check_design(xt=xt1,a=list(c(2,3,4),c(4,5,6)))

check_design(xt=c(0,1,2,4,6,8,24),
             groupsize=50,
             a=c(WT=70,DOSE=1000))

check_design(xt=c(0,1,2,4,6,8,24),
             groupsize=50,
             a=c(WT=70,DOSE=1000),m=2)

check_design(xt=c(0,1,2,4,6,8,24),
             groupsize=50,
             a=list(c(WT=70,DOSE=1000),c(DOSE=90,WT=200,AGE=45)),m=2)

check_design(xt=c(0,1,2,4,6,8,24),
             groupsize=50,
             a=list(c(WT=70,DOSE=1000),c(DOSE=90,WT=200,AGE=45)),m=2)

check_design(xt=c(0,1,2,4,6,8,24),
             groupsize=50,
             a=list(list(WT=70,DOSE=1000),list(DOSE=90,WT=200,AGE=45)),m=2)

