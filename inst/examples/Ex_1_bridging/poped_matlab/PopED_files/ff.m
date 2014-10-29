function [y,globalStructure]=ff(model_switch,xt,g,globalStructure)

    y=xt;

    %g = {CL,V,Dose}
    
    tvcl=g(1);
    tvv=g(2);
        
    emax=g(3);
    ec50=g(4);
    h=g(5);
    
    wt=g(6);    % covariate a(1)
      
    
    cl=tvcl+((emax*wt^h)/(ec50^h+wt^h));    
    v=g(2)*(wt/70);
    dose=1000*(wt/70);    
    
    y=((dose/v).*(exp(-cl/v.*xt)));

end 
