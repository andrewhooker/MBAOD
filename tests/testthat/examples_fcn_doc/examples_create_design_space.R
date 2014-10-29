library(PopED)
source("create_design_space.R")

design_1 <- create_design(xt=list(c(1,2,3,4,5),
                                  c(1,2,3,4)),
                          groupsize=c(50,20),
                          a=list(c(WT=70,DOSE=1000),
                                 c(WT=35,DOSE=1000)))

create_design_space(design_1)
create_design_space(design_1,maxni=10,maxxt=10,minxt=0)
create_design_space(design_1,maxni=10,minni=11) # should give an error
create_design_space(design_1,minni=15) # should give error 
create_design_space(design_1,maxni=10,mingroupsize=30) # should give error
create_design_space(design_1,maxni=10,mingroupsize=20,minxt=0) # should give an error
create_design_space(design_1,maxa=c(100,2000))
create_design_space(design_1,mina=c(10,20))



design_2 <- create_design(xt=list(c(1,2,3,4,5),
                                  c(1,2,3,4)),
                          groupsize=c(50,20))

create_design_space(design_2)




design_3 <- create_design(xt=list(c(1,2,3,4,5),
                                  c(1,2,3,4)),
                          groupsize=c(50,20),
                          a=list(c(WT=70,DOSE=1000),
                                 c(WT=35,DOSE=1000)),
                          x=list(c(SEX=1,DOSE_discrete=100),
                                 c(SEX=2,DOSE_discrete=200)))

create_design_space(design_3) 
create_design_space(design_3,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)))
create_design_space(design_3,x_space=list(SEX=c(0,2),DOSE_discrete=seq(100,400,by=20))) # should give an error

create_design_space(design_3,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),grouped_xt=c(1,2,3,4,5))
create_design_space(design_3,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),grouped_xt=c(1,2,3,4,6)) # should give error


create_design_space(design_3,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),use_grouped_xt=TRUE)

design_4 <- create_design(xt=list(c(1,2,3,4,5),
                                  c(1,2,3,4)),
                          groupsize=c(50,20),
                          a=list(c(WT=35,DOSE=1000)),
                          x=list(c(SEX=1,DOSE_discrete=100)))

create_design_space(design_4,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),use_grouped_a=T)

create_design_space(design_3,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),grouped_a=list(c(1,2),c(3,2)))

create_design_space(design_4,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),use_grouped_x=T)

create_design_space(design_4,x_space=list(SEX=c(1,2),DOSE_discrete=seq(100,400,by=20)),grouped_x=list(c(1,2),c(3,2)))
