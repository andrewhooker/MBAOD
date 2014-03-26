library(grid)
library(ggplot2)
library(reshape2)


setwd("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/examples/")

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/R",trace=FALSE)



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


#load("resultI_5_2_nsteps_4_nid_20_missp_0.01-25-5.Rdata")
#rm(resall)

#name=paste("4_2_nsteps_",nsteps,"_nid_",nid,"_missp_",paste(m, collapse="-"),sep="")

name.fix <- "4_nsteps_2_nid_20_rep_100_missp_0.01-25-5" 
name.fix <- "4_nsteps_2_nid_20_rep_100_missp_2-5-5"
name.fix <- "4_nsteps_2_nid_20_rep_100_missp_2-50-5"
name.fix <- "4_nsteps_2_nid_20_rep_100_missp_2-25-5"
name.fix <- "5_nsteps_4_nid_20_rep_100_missp_2-25-5"

summary.fix <- summaryResults(name=name.fix)   
ree.fix <- extract.ree(summary.fix)
stack.fix <- stack(ree.fix,select=c(thCl,thV,thMax,thE50,thHill))
#stack.fix <- stack(ree.fix,select=c(thCl,thV,thMax,thE50,thHill,omCL,omV,sigP,sigA))
stack.fix$type <- "FIXED-OD"
stack.fix$type <- "AOD: 1 group per cohort"

name.aod <- "5_nsteps_4_nid_20_rep_100_missp_0.01-25-5" 
name.aod <- "5_nsteps_4_nid_20_rep_100_missp_2-5-5"
name.aod <- "5_nsteps_4_nid_20_rep_100_missp_2-50-5"
name.aod <- "5_nsteps_4_nid_20_rep_100_missp_2-25-5"
name.aod <- "5_nsteps_4_nid_7_7_6_rep_100_missp_2-25-5"

summary.aod <- summaryResults(name=name.aod)   
ree.aod <- extract.ree(summary.aod)
stack.aod <- stack(ree.aod,select=c(thCl,thV,thMax,thE50,thHill))
#stack.aod <- stack(ree.aod,select=c(thCl,thV,thMax,thE50,thHill,omCL,omV,sigP,sigA))
stack.aod$type <- "AOD"
stack.aod$type <- "AOD: 4 groups per cohort"

foo <- rbind(stack.aod,stack.fix)

p <- ggplot(data=foo,aes(x=ind,y=values))
p +  geom_boxplot() + geom_jitter(position=position_jitter(width=0.05)) + facet_grid(~type) + 
  #scale_y_continuous(name="REE") + 
  coord_cartesian(ylim = c(-100, 100)) + 
  #scale_x_discrete(name="Parameter") +
  xlab("Parameter") +
  ylab("REE (%)") +
  theme(title = element_text(size=25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        strip.text=element_text(size=20))

final.par.fix <- summary.fix[[2]][!duplicated(summary.fix[[2]]$rep,fromLast=T),]
final.par.aod <- summary.aod[[2]][!duplicated(summary.aod[[2]]$rep,fromLast=T),]

auc.fix <- compute.auc(final.par.fix)
auc.aod <- compute.auc(final.par.aod)

##auc.vals <- auc.aod
##quantile(auc.vals[auc.vals$wt==1,]$obsAUC,probs=c(0.05,0.25,0.5,0.75,0.95))

auc.fix$type <- "FIXED-OD"
auc.aod$type <- "AOD"

auc.fix$type <- "AOD: 1 group per cohort"
auc.aod$type <- "AOD: 4 groups per cohort"

auc.both <- rbind(auc.fix,auc.aod) 

p <- ggplot(data=auc.both)
p + geom_line(aes(x=wt,y=trueAUC)) + geom_point(aes(x=wt,y=obsAUC),alpha=0.05) + scale_y_continuous(name="AUC") + facet_grid(~type)

p + geom_line(aes(x=wt,y=trueAUC),col="blue") + 
  #geom_point(aes(x=wt,y=obsAUC),alpha=0.05)+
  stat_summary(aes(x=wt,y=obsAUC),geom="ribbon",fun.data="median_hilow",alpha=0.3, fill="red")+
  scale_y_continuous(name="AUC") + facet_grid(~type) +
  theme(title = element_text(size=25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        strip.text=element_text(size=20))


p + geom_line(aes(x=wt,y=trueAUC)) + geom_point(aes(x=wt,y=obsAUC),alpha=0.05)+
  stat_summary(aes(x=wt,y=obsAUC),geom="ribbon",fun.ymin="min",fun.ymax="max")+
  scale_y_continuous(name="AUC") + facet_grid(~type) 

##########

EM <- c()
for(i in 1:70){
  ##j=1
  ##i=1
  wt=i
  trueEMAX=(1+ (2 * wt^5)/ (25^5 + wt^5))
  missEMAX=(1+ (2 * wt^5)/ (5^5 + wt^5))
  EM <- rbind(EM,c(wt=wt,trueEMAX=trueEMAX,missEMAX=missEMAX))
}
emax.curve <- as.data.frame(EM)

emax.curve$COV1 <- NA
emax.curve$COV1[70]=emax.curve$trueEMAX[70]

p1 <- ggplot(data=emax.curve) +
  geom_line(aes(x=wt,y=trueEMAX),col="blue") + geom_line(aes(x=wt,y=missEMAX),col="red") +
  geom_point(aes(x=wt,y=COV1),cex=5) + 
  xlab("WT") +
  ylab("Clearance") +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=20))
p1


emax.curve$COV2 <- NA
emax.curve$COV2[1:4] <- summary.fix[[1]][1:4,]$cov

emax.curve$COV2.EMt <- (1+ (2 * emax.curve$COV2^5)/ (25^5 + emax.curve$COV2^5))
emax.curve$COV2.EMm <- (1+ (2 * emax.curve$COV2^5)/ (5^5 + emax.curve$COV2^5))


p2 <- ggplot(data=emax.curve) +
  geom_line(aes(x=wt,y=trueEMAX),col="blue") + geom_line(aes(x=wt,y=missEMAX),col="red") +
  #geom_point(aes(x=wt,y=COV1),cex=5) +
  geom_point(aes(x=COV2,y=COV2.EMt),cex=5) +
  #geom_point(aes(x=COV2,y=COV2.EMm),cex=5) + 
  xlab("WT") +
  ylab("Clearance")+
  #ggtitle("Cohort 2")
  theme(axis.title = element_text(size=20),
      axis.text = element_text(size=20))
p2

p3 <- ggplot(data=emax.curve) +
  geom_line(aes(x=wt,y=trueEMAX),col="blue") + geom_line(aes(x=wt,y=missEMAX),col="red") +
  geom_point(aes(x=wt,y=COV1),cex=5) +
  geom_point(aes(x=COV2,y=COV2.EMt),cex=5,pch="X") +
  #geom_point(aes(x=COV2,y=COV2.EMm),cex=5) + 
  xlab("WT") +
  ylab("Clearance")+
  ggtitle("Cohort 2")
p2

em.curve <- function(params=c(1,2,25,5),wt=wt){
  emax.curve <- params[[1]]+ (params[[2]] * wt^params[[4]])/(params[[3]]^params[[4]] + wt^params[[4]])
  return(emax.curve)
}

pars.1.fix <- subset(summary.fix[[2]],grp==2 & rep==1)
params.1.fix <- with(pars.1.fix,c(thCl,thMax,thE50,thHill))
emax.curve$em.co.2.fix <- unlist(lapply(1:70,"em.curve",params=params.1.fix))

pars.1 <- subset(summary.aod[[2]],grp==2 & rep==1)
pars.2 <- subset(summary.aod[[2]],grp==3 & rep==1)
pars.3 <- subset(summary.aod[[2]],grp==4 & rep==1)

params.1 <- with(pars.1,c(thCl,thMax,thE50,thHill))
params.2 <- with(pars.2,c(thCl,thMax,thE50,thHill))
params.3 <- with(pars.3,c(thCl,thMax,thE50,thHill))

emax.curve$em.co.2 <- unlist(lapply(1:70,"em.curve",params=params.1))
emax.curve$em.co.3 <- unlist(lapply(1:70,"em.curve",params=params.2))
emax.curve$em.co.4 <- unlist(lapply(1:70,"em.curve",params=params.3))

emax.curve$COV2 <- NA
emax.curve$COV2[1] <- summary.aod[[1]][2,]$cov
emax.curve$COV2.cl <- em.curve(wt=emax.curve$COV2)

emax.curve$COV3 <- NA
emax.curve$COV3[1] <- summary.aod[[1]][3,]$cov
emax.curve$COV3.cl <- em.curve(wt=emax.curve$COV3)

emax.curve$COV4 <- NA
emax.curve$COV4[1] <- summary.aod[[1]][4,]$cov
emax.curve$COV4.cl <- em.curve(wt=emax.curve$COV4)

adult.data$trueEMAX

data.frame(trueEMAX=emax.curve)

p3 <- ggplot(data=emax.curve) +
  geom_line(aes(x=wt,y=trueEMAX),col="blue") +
  #geom_line(aes(x=wt,y=missEMAX),col="red") +
  geom_line(aes(x=wt,y=em.co.2.fix),col="red") +
  #geom_line(aes(x=wt,y=em.co.2),col="red") +
  #geom_line(aes(x=wt,y=em.co.3),col="red") +
  #geom_line(aes(x=wt,y=em.co.4),col="red") +
  #geom_point(aes(x=wt,y=COV1),cex=5) +  
  #geom_point(aes(x=COV2,y=COV2.cl),cex=5) +
  #geom_point(aes(x=COV3,y=COV3.cl),cex=5) +
  #geom_point(aes(x=COV4,y=COV4.cl),cex=5) +
  #geom_point(aes(x=COV2,y=COV2.EMm),cex=5) + 
  geom_point(aes(x=COV2,y=COV2.EMt),cex=5) +
  xlab("WT") +
  ylab("Clearance")+
  #ggtitle("")+
  #ggtitle("Cohort 1")+
  #ggtitle("Cohort 2")+
  #ggtitle("Cohort 3")+
  theme(axis.title = element_text(size=20),
        title = element_text(size=25),
        axis.text = element_text(size=20))
p3



quartz()
vp1 <- viewport(width = 0.5, height = 1,x=0.25,y=0.5)
vp2 <- viewport(width = 0.5, height = 1,x=0.75,y=0.5)
print(p1, vp = vp1)
print(p2, vp = vp2)

quartz()
vp1 <- viewport(width = 0.5, height = 1,x=0.25,y=0.5)
vp2 <- viewport(width = 0.5, height = 1,x=0.75,y=0.5)
print(p1, vp = vp1)
print(p2, vp = vp2)

vp2 <- viewport(width = 0.18, height = 1, x = 0.26, y = 0.5)
vp3 <- viewport(width = 0.25, height = 1, x = 0.46, y = 0.5)
vp4 <- viewport(width = 0.25, height = 1, x = 0.70, y = 0.5)
vp5 <- viewport(width = 0.20, height = 1, x = 0.91, y = 0.5)

pdf("plots/plot.pdf", width=29, height=20)

print(p1, vp = vp1)
print(p2, vp = vp2)
print(p3, vp = vp3)
print(p4, vp = vp4)
print(p5, vp = vp5)


+ facet_grid(~type)

########


m <- ggplot(movies, aes(y=votes, x=year))
(m <- m + geom_point())

# The default summary isn't that useful
m + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
m + stat_summary(geom="ribbon", fun.data="median_hilow")




p + geom_line(aes(x=wt,y=trueAUC)) + geom_polygon(aes(x=wt,y=obsAUC))
  geom_boxplot() +
  geom_jitter(position=position_jitter(width=0.05)) + facet_grid(~type)


      med=median(obsAUC)
      qlo=quantile(obsAUC,.25)
      qhi=quantile(obsAUC,.75)
      temp<-data.frame(description=meta$description,
                       name=count,
                       ngroups=meta$ngroups,
                       nsubject=min(unique(meta$nsubjects[[1]])),
                       wt=i,
                       med=med,
                       qlo=qlo,
                       qhi=qhi,
                       true=trueAUC)
      temp<-melt(temp, id.var=c("description","name","ngroups","nsubject","wt"))
      resAUC<-rbind(resAUC, temp)
    }


results <- as.list(dir(pattern="resultI_._2"))
res <- compareDesigns(results)

#################################################################
res<-compareDesigns()
#################################################################


#################################################################
# ALL
  
p1<-
  ggplot(data=res$allMeta)+
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_text(aes(label=value,x=1, y=xax), face="bold", size=2.5, color="black") + 
  scale_y_continuous(breaks=seq(0,30,1), name="Subscenario") +  
  scale_x_continuous(name="Number", labels="") +
  facet_grid(description~variable, scales = "free_y",  space = "free_y")+ 
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

p2<-
  ggplot(data=res$polygMis) +
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_vline(aes(x=0), size=.5, colour="grey")+ theme_bw() +
  geom_polygon( aes(x=x, y=y, group=name), colour="blue", size=.4, fill="blue", alpha=.6)+
  scale_x_continuous(name="Relative error misspecification", limits=c(-2,2), breaks=seq(-2,2,1))+
  scale_y_continuous(name="", breaks=seq(1,30,1))+
  facet_grid(description~variable, scale="free_y", space="free_y")+
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

 p3<-
  ggplot(data=res$polygBias) +
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_vline(aes(x=0), size=.5, colour="grey")+ theme_bw() +
  geom_polygon(aes(x=x, y=y, group=name), colour="black", size=.4, fill="white", alpha=.6)+
  geom_point(aes(x=med, y=name, group=name), size=1, colour="blue")+
  scale_x_continuous(name="Relative error", breaks=seq(-20,20,5))+
  scale_y_continuous(name="", breaks=seq(1,30,1))+
  coord_cartesian(xlim=c(-20,20))+
  facet_grid(description~variable, scale="free_y", space="free_y")+
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size = 12, face="bold"),
       strip.text.y = theme_text(size = 12, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")


p4<-
  ggplot(data=res$allTimes, aes(time,yaxis)) +
  geom_polygon(data=res$polygTimes, aes(x=x, y=y, group=name), fill="grey", alpha=.5) +
  geom_point(aes(colour=group), size=1.5) + coord_cartesian(xlim=c(-0.5,24.5)) +
  scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
  scale_x_continuous(name="Sampling time (h)", breaks=seq(0,24,3) ) +
  facet_grid(description~label, scale="free_y", space="free_y")  +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size = 12, face="bold"),
       strip.text.y = theme_text(size = 12, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

  
p5<- ggplot(data=res$allCov, aes(x=value, y=name))+
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_point(aes(colour=variable))+
  scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
  scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,10) )+
  facet_grid(description~label, scale="free_y", space="free_y") +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90),
       strip.text.x = theme_text(size = 12, face="bold"),
       strip.text.y = theme_text(size = 12, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

vp1 <- viewport(width = 0.18, height = 1, x = 0.085, y = 0.5)
vp2 <- viewport(width = 0.18, height = 1, x = 0.26, y = 0.5)
vp3 <- viewport(width = 0.25, height = 1, x = 0.46, y = 0.5)
vp4 <- viewport(width = 0.25, height = 1, x = 0.70, y = 0.5)
vp5 <- viewport(width = 0.20, height = 1, x = 0.91, y = 0.5)

pdf("plots/plot.pdf", width=29, height=20)

print(p1, vp = vp1)
print(p2, vp = vp2)
print(p3, vp = vp3)
print(p4, vp = vp4)
print(p5, vp = vp5)

dev.off()

#system("evince plots/plot.pdf")
  

###################################################################################
# GROUPS
  
tempAUC<-  res$resAUC
tempAUC<-tempAUC[tempAUC$variable %in% c("med","true"),]
groups<-unique(tempAUC$description)[2:3]
tempAUC<-tempAUC[tempAUC$description %in% groups,]
tempAUC<-tempAUC[tempAUC$ngroup < 4,]

tempPAUC<-res$polygAUC
tempPAUC<-tempPAUC[tempPAUC$description %in% groups,]
tempPAUC<-tempPAUC[tempPAUC$ngroup < 4,]

tempMIS<-res$polygMis
tempMIS<-tempMIS[tempMIS$description %in% groups[2],]
tempMIS<-tempMIS[tempMIS$ngroup < 4,]

tempCov<-res$allCov
tempCov<-tempCov[tempCov$description %in% groups,]
tempCov<-tempCov[tempCov$ngroup < 4,]


p1<-
  ggplot(data=tempMIS) +
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_vline(aes(x=0), size=.5, colour="grey")+ theme_bw() +
  geom_polygon( aes(x=x, y=y, group=name), colour="blue", size=.4, fill="blue", alpha=.6)+
  scale_x_continuous(name="Relative error misspecification")+
  scale_y_continuous(name="Misspecification", breaks=seq(1,30,1))+
  facet_grid(ngroups+name~variable, scale="free_y", space="free_y")  +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

p2<-
  ggplot()+
  geom_polygon(data=tempPAUC, aes(x=x,y=y,group=name), alpha=.2)  +
  geom_line(data=tempAUC, aes(x=wt,y=value, group=variable, colour=variable))+
  scale_x_continuous(name="Covariate value (wt)")+
  scale_y_continuous(name="AUC")+
  facet_grid(ngroups+name~description) +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank())



 
p3<-
  ggplot(data=tempCov, aes(x=value, y=name))+
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_point(aes(colour=variable), size=3)+
  scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
  scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,10) )+
  facet_grid(ngroups+name~description, scale="free_y", space="free_y") +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")



vp1 <- viewport(width = 0.18, height = 1, x = 0.085, y = 0.5)
vp2 <- viewport(width = 0.44, height = 1, x = 0.41, y = 0.5)
vp3 <- viewport(width = 0.35, height = 1, x = 0.78, y = 0.5)


pdf("plots/plotAUC-group.pdf", width=12, height=7)

print(p1, vp = vp1)
print(p2, vp = vp2)
print(p3, vp = vp3)

dev.off()

# system("evince plots/plotAUC-group.pdf")
  

###################################################################################
# NUMBER PATIENTS
  
tempAUC<-  res$resAUC
tempAUC<-tempAUC[tempAUC$variable %in% c("med","true"),]
groups<-unique(tempAUC$description)[5:6]
tempAUC<-tempAUC[tempAUC$description %in% groups,]
tempAUC<-tempAUC[tempAUC$nsubject < 11,]

tempPAUC<-res$polygAUC
tempPAUC<-tempPAUC[tempPAUC$description %in% groups,]
tempPAUC<-tempPAUC[tempPAUC$nsubject < 11,]

tempMIS<-res$polygMis
tempMIS<-tempMIS[tempMIS$description %in% groups[2],]
tempMIS<-tempMIS[tempMIS$nsubject < 11,]

tempCov<-res$allCov
tempCov<-tempCov[tempCov$description %in% groups,]
tempCov<-tempCov[tempCov$nsubject < 11,]


p1<-
  ggplot(data=tempMIS) +
  geom_hline(aes(yintercept=name), colour="gray")+
  geom_vline(aes(x=0), size=.5, colour="grey")+ theme_bw() +
  geom_polygon( aes(x=x, y=y, group=name), colour="blue", size=.4, fill="blue", alpha=.6)+
  scale_x_continuous(name="Relative error misspecification")+
  scale_y_continuous(name="Misspecification", breaks=seq(1,30,1))+
  facet_grid(nsubject+name~variable, scale="free_y", space="free_y")  +
  theme_bw() +
  opts(axis.ticks.margin=unit(0.25,"lines"),
       axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
       axis.title.y = theme_text(face="bold", size=8, angle=90), 
       strip.text.x = theme_text(size=8, face="bold"),
       strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.position="none")

p2<-
  ggplot()+
  geom_polygon(data=tempPAUC, aes(x=x,y=y,group=name), alpha=.2)  +
  geom_line(data=tempAUC, aes(x=wt,y=value, group=variable, colour=variable))+
  scale_x_continuous(name="Covariate value (wt)")+
  scale_y_continuous(name="AUC")+
  facet_grid(nsubject+name~description) +
  theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank())



 
p3<-
  ggplot(data=tempCov, aes(x=value, y=name))+
    geom_hline(aes(yintercept=name), colour="gray")+
      geom_point(aes(colour=variable), size=3)+
        scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
          scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,10) )+
            facet_grid(ngroups+name~description, scale="free_y", space="free_y") +
                      theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     legend.position="none")




  pdf("plots/plotAUC-nsubject.pdf", width=12, height=7)
  
  vp1 <- viewport(width = 0.18, height = 1, x = 0.085, y = 0.5)
  vp2 <- viewport(width = 0.44, height = 1, x = 0.41, y = 0.5)
  vp3 <- viewport(width = 0.35, height = 1, x = 0.78, y = 0.5)

  print(p1, vp = vp1)
  print(p2, vp = vp2)
  print(p3, vp = vp3)
  
  dev.off()

 # system("evince plots/plotAUC-nsubject.pdf")
  

###################################################################################
#AUC n groups FIX vs ADAPTIVE
  
tempAUC<-  res$resAUC
tempAUC<-tempAUC[tempAUC$variable %in% c("med","true"),]

groups<-unique(tempAUC$description)[4]
tempAUC<-tempAUC[tempAUC$description %in% groups,]
tempPAUC<-res$polygAUC
tempPAUC<-tempPAUC[tempPAUC$description %in% groups,]

tempMIS<-res$polygMis
tempMIS<-tempMIS[tempMIS$description %in% groups,]

tempCov<-res$allCov
tempCov<-tempCov[tempCov$description %in% groups,]

tempTimes<-res$allTimes
tempTimes<-tempTimes[tempTimes$description %in% groups,]
tempPolygtimes<-res$PolygTimes
tempPolygtimes<-tempPolygtimes[tempPolygtimes %in% groups,]



p1<-
  ggplot()+
    geom_polygon(data=tempPAUC, aes(x=x,y=y,group=name), alpha=.2)  +
      geom_line(data=tempAUC, aes(x=wt,y=value, group=variable, colour=variable))+
        scale_x_continuous(name="Covariate value (wt)")+
          scale_y_continuous(name="AUC")+
            facet_grid(name~description) +
                      theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank())



 
p2<-
  ggplot(data=tempCov, aes(x=value, y=name))+
    geom_hline(aes(yintercept=name), colour="gray")+
      geom_point(aes(colour=variable), size=3)+
        scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
          scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,10) )+
            facet_grid(name~description, scale="free_y", space="free_y") +
                      theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     legend.position="none")

p3<- ggplot(data=tempTimes, aes(time,yaxis)) +
     # geom_polygon(data=tempPolygtimes, aes(x=x, y=y), fill="grey", alpha=.5) +
        geom_point(aes(colour=group), size=3) + coord_cartesian(xlim=c(-0.5,24.5)) +
          scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
            scale_x_continuous(name="Sampling time (h)", breaks=seq(0,24,3) ) +
              facet_grid(name~description, scale="free_y", space="free_y")  +
                theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     legend.position="none")



  pdf("plots/plotAUC-timecov.pdf", width=12, height=7)
  
  vp1 <- viewport(width = 0.45, height = 1, x = 0.22, y = 0.5)
  vp2 <- viewport(width = 0.30, height = 1, x = 0.58, y = 0.5)
  vp3 <- viewport(width = 0.25, height = 1, x = 0.85, y = 0.5)


  print(p1, vp = vp1)
  print(p2, vp = vp2)
  print(p3, vp = vp3)

  
  dev.off()

#  system("evince plots/plotAUC-timecov.pdf")
  
###################################################################################
# FIX versus ADAPTIVE
  
tempAUC<-  res$resAUC
tempAUC<-tempAUC[tempAUC$variable %in% c("med","true"),]

groups<-unique(tempAUC$description)[1:2]
tempAUC<-tempAUC[tempAUC$description %in% groups,]
tempAUC<-tempAUC[tempAUC$description==groups[1] | (tempAUC$description==groups[2] & tempAUC$name==14),]

tempPAUC<-res$polygAUC
tempPAUC<-tempPAUC[tempPAUC$description %in% groups,]
tempPAUC<-tempPAUC[tempPAUC$description==groups[1] | (tempPAUC$description==groups[2] & tempPAUC$name==14),]

tempMIS<-res$polygMis
tempMIS<-tempMIS[tempMIS$description %in% groups,]
tempMIS<-tempMIS[tempMIS$description==groups[1] | (tempMIS$description==groups[2] & tempMIS$name==14),]

tempCov<-res$allCov
tempCov<-tempCov[tempCov$description %in% groups,]
tempCov<-tempCov[tempCov$description==groups[1] | (tempCov$description==groups[2] & tempCov$name==14),]

tempTimes<-res$allTimes
tempTimes<-tempTimes[tempTimes$description %in% groups,]
tempPolygtimes<-res$PolygTimes
tempPolygtimes<-tempPolygtimes[tempPolygtimes %in% groups,]


p1<-
  ggplot()+
    geom_polygon(data=tempPAUC, aes(x=x,y=y,group=name), alpha=.2)  +
      geom_line(data=tempAUC, aes(x=wt,y=value, group=variable, colour=variable))+
        scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,20))+
          scale_y_continuous(name="AUC")+
            facet_grid(name~description) +
                      theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank())

p2<-
  ggplot(data=tempCov, aes(x=value, y=name))+
    geom_hline(aes(yintercept=name), colour="gray")+
      geom_point(aes(colour=variable), size=3)+
        scale_y_continuous(name="", breaks=c(1,2))+theme_bw() + 
          scale_x_continuous(name="Covariate value (wt)", breaks=seq(0,70,10) )+
            facet_grid(name~description, scale="free_y", space="free_y") +
                      theme_bw() +
                   opts(axis.ticks.margin=unit(0.25,"lines"),
                     axis.title.x = theme_text(face="bold", size=8, vjust=-0.5),
                     axis.title.y = theme_text(face="bold", size=8, angle=90), 
                     strip.text.x = theme_text(size=8, face="bold"),
                     strip.text.y = theme_text(size=8, face="bold", angle=-90),                     
                     panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     legend.position="none")

  pdf("plots/plotAUC-optimal.pdf", width=7, height=5)
  
  vp1 <- viewport(width = 0.50, height = 1, x = 0.25, y = 0.5)
  vp2 <- viewport(width = 0.60, height = 1, x = 0.70, y = 0.5)

  print(p1, vp = vp1)
  print(p2, vp = vp2)

  dev.off()

 # system("evince plots/plotAUC-optimal.pdf")
  


