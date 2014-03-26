evalFixedDesign<-function(name, description,samples,
                          times=list(c(1,2,3),c(1,2,4), c(1,2,3)),
                          cov=list(20,30,40),
                          nid=list(10,10,10),
                          ngroups, nsteps,
                          misspec){
  
  prev <- list(list(dose = list(1000),
                    cov = cov,
                    nid = nid,
                    samplingtimes = times))
  
  result <- evalDesign(prev = prev,
                       name = name,
                       description = description,
                       samples = samples,
                       ngroups = ngroups,
                       nsteps = nsteps,
                       misspec = misspec)
  
  return(result)
}

