devtools::load_all("/Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD")

path <- "Example_1_rep_100"

create_plot_files <- T 

#################################
######## for all plots
#################################

load(file.path(path,"results_all.Rdata"))

source("PopED_files/poped.mod.PK.1.comp.maturation.R")

design_list <- results_all[grep("^iteration",names(results_all))]

all_designs <- combine_designs(design_list,design_name = "final_design")

model = list(
  ff_file="PK.1.comp.maturation.ff",
  fError_file="feps.add.prop",
  fg_file="PK.1.comp.maturation.fg"
)

parameters_true=list(
  bpop=c(CL=1,V=20,EMAX=2,EC50=25,HILL=5),
  d=c(0.05,0.05),
  sigma=c(0.015,0.0015)
)

#################################
######## optimized designs
#################################

poped.db <- do.call(create.poped.database,c(all_designs,model,parameters_true))

plot1 <- plot_model_prediction(poped.db,y_lab="Concentration") + theme(legend.position="none")
plot1

if(create_plot_files){ 
  pdf(file.path(path,"optimized_designs.pdf"),width=7, height=5,onefile=F)
  plot1
  dev.off()
}

#################################
######## PARAMETER ESTIMATES 
#################################

parameters_true_sd <- parameters_true
parameters_true_sd$d <- sqrt(parameters_true_sd$d) 
parameters_true_sd$sigma <- sqrt(parameters_true_sd$sigma) 

plot2 <- plot_parameter_estimates(results_all,unlist(parameters_true_sd))
plot2

plot2a <- plot2 + coord_cartesian(xlim = NULL, ylim= c(-50,50))
plot2a

if(create_plot_files){
  pdf(file.path(path,"parameter_estimates%03d.pdf"),width=7,height=5,onefile=F)
  plot2  
  plot2a
  dev.off()
} 

#################################
######## VPC of IPRED from estimated models and true model
#################################

design_1 = list(
  groupsize = 200,
  m=1,
  a   = 35,
  xt = c(0.5,1,2,3,6,12,24)
)

design_2 = list(
  groupsize = 200,
  m=4,
  a   = rbind(10, 35, 55, 70),
  xt = c(0.5,1,2,3,6,12,24)
)

vpc1 <- mbaod_vpc(design_1, 
          model, 
          parameters_true, 
          results_all)
vpc1

vpc2 <- mbaod_vpc(design_2, 
          model, 
          parameters_true, 
          results_all, 
          separate.groups=T)
vpc2

if(create_plot_files){
  pdf(file=file.path(path,"vpc%03d.pdf"),width=7,height=5,onefile=F)
  vpc1  
  vpc2
  dev.off()
} 

# #################################
# ######## Clearance plots (specific for this problem) -- visualization of WT choices
# #################################

CL_mod <- function(params=list(BASE=1,EMAX=2,E50=25,HILL=5),IDV){
  with(params,{
    vals <- BASE+ (EMAX*IDV^HILL)/(E50^HILL + IDV^HILL)
    return(vals)
  })
}

df <- data.frame(WT=0:70)
df$CL=CL_mod(IDV=df$WT)

#all_designs
design_list <- results_all[grep("^iteration",names(results_all))]
all_designs <- combine_designs(design_list,design_name = "final_design")

df.2 <- data.frame(all_designs$a)
df.2$CL=CL_mod(IDV=df.2$WT)
nrep <- length(grep("^iteration",names(results_all)))
ncohort <- size(df.2,1)/nrep
df.2$Cohort=as.factor(rep(1:ncohort,nrep))  

p <- ggplot(data=df, aes(x=WT,y=CL))
p <- p+geom_line()
p <- p+geom_point(data=df.2,aes(color=Cohort),size=4)
p

if(create_plot_files){
  pdf(file.path(path,"clearance_plots%03d.pdf"),width=7,height=5,onefile=F)
  p
  dev.off()
} 

