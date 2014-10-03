library(PopED)

PK.1.comp.maturation.fg <- function(x,a,bpop,b,bocc){
  ## -- parameter definition function 
  parameters=c( CL=bpop[1]*exp(b[1]),
                V=bpop[2]*exp(b[2]),
                EMAX=bpop[3],
                EC50=bpop[4],
                HILL=bpop[5],
                WT=a[1])
  return( parameters ) 
}


PK.1.comp.maturation.ff <- function(model_switch,xt,parameters,poped.db){
  with(as.list(parameters),{
    y=xt
    
    CL=CL+(EMAX*WT**HILL)/(EC50**HILL+WT**HILL)
    V=V*(WT/70)
    DOSE=1000*(WT/70)
    y = DOSE/V*exp(-CL/V*xt) 
       
    return(list( y= y,poped.db=poped.db))
  })
}

