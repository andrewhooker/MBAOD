function pop_params = sfg(x,a,bpop,b,bocc)

% -- Auto generated parameter definition file --
% -- Created: 04-Apr-2012 19:13:30
% -- {CL,V,Wt} --


pop_params=[ bpop(1)*exp(b(1))	%g(1)=CL
 bpop(2)*exp(b(2))	%g(2)=V 
 bpop(3) % emax
 bpop(4)  % ec50
 bpop(5) % hill
 a(1)];	% Wt

end