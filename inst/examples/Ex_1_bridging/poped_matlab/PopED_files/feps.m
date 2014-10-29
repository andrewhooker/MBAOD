function [y,globalStructure] = feps(model_switch,xt,g,epsi,globalStructure)
% -- Auto generated error model file for model One compartment IV bolus dose --
% -- Created: 04-Apr-2012 19:13:30
[y,globalStructure]=feval(globalStructure.ff_pointer,model_switch,xt,g,globalStructure);
%y = y.*(1+epsi(:,1));
y = y.*(1+epsi(:,1))+epsi(:,2);

end