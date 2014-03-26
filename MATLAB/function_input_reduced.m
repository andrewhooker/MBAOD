popedInput.strPopEDVersion='2.12';
popedInput.nx=0;
popedInput.ndocc=0;
popedInput.NumOcc=0; % -- Number of occassions --
popedInput.iFOCENumInd=1000; % -- Num indivduals in each conditional step --
popedInput.bUseRandomSearch=1; % -- Use random search (1=true, 0=false) --
popedInput.bUseStochasticGradient=0; % -- Use Stochastic Gradient search (1=true, 0=false) --
popedInput.bUseLineSearch=0; % -- Use Line search (1=true, 0=false) --
popedInput.bUseExchangeAlgorithm=0; % -- Use Exchange algorithm (1=true, 0=false) --
popedInput.bUseBFGSMinimizer=0; % -- Use BFGS Minimizer (1=true, 0=false) --
popedInput.line_optx=zeros(1,0)'; % -- Vector for line search on discrete design variables (1=true,0=false) --
popedInput.dSeed=-1; % -- The seed number used for optimization and sampling --
popedInput.design.discrete_x=cell(0,1)'; % -- Cell defining the discrete variables --
popedInput.bUseGrouped_x=0; % -- Use grouped discrete design variables (1=true, 0=false) --
popedInput.ff_file='ff.m'; % -- Filname and path of the model file --
popedInput.fg_file='sfg.m'; % -- Filname and path of the g parameter file --
popedInput.fError_file='feps.m'; % -- Filname and path of the error model file --
popedInput.strUserDistributionFile='';% -- Filname and path for the user defined distributions --
popedInput.strEDPenaltyFile='';% -- Filname and path for the ED penalty function, empty string means no penalty --
popedInput.strAutoCorrelationFile='';% -- Filname and path for the Autocorrelation function, empty string means no autocorrelation --
popedInput.modtit='One compartment IV bolus dose';% -- The model title --
popedInput.bShowGraphs=0;% -- Use graph output during search --
popedInput.use_logfile=0;% -- If a log file should be used (0=false, 1=true) --
popedInput.output_file='';
popedInput.output_function_file='';
popedInput.strIterationFileName='';% -- Filename and path for storage of current optimal design --
popedInput.m1_switch=0;% -- Method used to calculate M1 (0=Complex difference, 1=Central difference, 20=Analytic derivative, 30=Automatic differentiation) --
popedInput.m2_switch=0;% -- Method used to calculate M2 (0=Central difference, 1=Central difference, 20=Analytic derivative, 30=Automatic differentiation) --
popedInput.hle_switch=0;% -- Method used to calculate linearization of residual error (0=Complex difference, 1=Central difference, 30=Automatic differentiation) --
popedInput.gradff_switch=0;% -- Method used to calculate the gradient of the model (0=Complex difference, 1=Central difference, 20=Analytic derivative, 30=Automatic differentiation) --
popedInput.gradfg_switch=0;% -- Method used to calculate the gradient of the parameter vector g (0=Complex difference, 1=Central difference, 20=Analytic derivative, 30=Automatic differentiation) --
popedInput.bLHS=0;% -- Use normal random sampling or Latin Hypercube sampling, 0=Random Sampling, 1=LatinHyperCube --
popedInput.ourzero=1e-05;% -- Value to interpret as zero in design --
popedInput.rsit_output=5;% -- Number of iterations in random search between screen output --
popedInput.sgit_output=1;% -- Number of iterations in stochastic gradient search between screen output --
popedInput.hm1=0.0001;% -- Step length of derivative of linearized model w.r.t. typical values --
popedInput.hlf=0.0001;% -- Step length of derivative of model w.r.t. g --
popedInput.hlg=0.0001;% -- Step length of derivative of g w.r.t. b --
popedInput.hm2=0.0001;% -- Step length of derivative of variance w.r.t. typical values --
popedInput.hgd=0.0001;% -- Step length of derivative of OFV w.r.t. time --
popedInput.hle=0.0001;% -- Step length of derivative of model w.r.t. sigma --
popedInput.AbsTol=1e-10;% -- The absolute tolerance for the diff equation solver --
popedInput.RelTol=1e-10;% -- The relative tolerance for the diff equation solver --
popedInput.iDiffSolverMethod=0;% -- The diff equation solver method, 0=ode45, 1=ode15s --
popedInput.bUseMemorySolver=0;% -- If the differential equation results should be stored in memory or not --
popedInput.iFIMCalculationType=0;% -- Fisher Information Matrix type (0=Full FIM, 1=Reduced FIM) --
popedInput.rsit=25;% -- Number of Random search iterations --
popedInput.sgit=5;% -- Number of Stochastic gradient search iterations --
popedInput.intrsit=100;% -- Number of Random search iterations when discrete optimization --
popedInput.intsgit=50;% -- Number of Stochastic Gradient search iterations when discrete optimization --
popedInput.maxrsnullit=50;% -- Iterations until adaptive narrowing in random search --
popedInput.convergence_eps=1e-60;% -- Stoachstic Gradient convergence value, (difference in OFV for D-optimal, difference in gradient for ED-optimal) --
popedInput.rslxt=4;% -- Random search locality factor for sample times --
popedInput.rsla=4;% -- Random search locality factor for covariates --
popedInput.cfaxt=0.001;% -- Stochastic Gradient search first step factor for sample times --
popedInput.cfaa=0.001;% -- Stochastic Gradient search first step factor for covariates --
popedInput.bGreedyGroupOpt=0;% -- Use greedy algorithm for group assignment optimization --
popedInput.EACriteria=1;% -- Exchange Algorithm Criteria, 1 = Modified, 2 = Fedorov --
popedInput.EAStepSize=0.01;% -- Exchange Algorithm StepSize --
popedInput.EANumPoints=0;% -- Exchange Algorithm NumPoints --
popedInput.EAConvergenceCriteria=1e-20;% -- Exchange Algorithm Convergence Limit/Criteria --
popedInput.bEANoReplicates=0;% -- Avoid replicate samples when using Exchange Algorithm --
popedInput.BFGSConvergenceCriteriaMinStep=1e-08;% -- BFGS Minimizer Convergence Criteria Minimum Step --
popedInput.BFGSProjectedGradientTol=0.0001;% -- BFGS Minimizer Convergence Criteria Normalized Projected Gradient Tolerance --
popedInput.BFGSTolerancef=0.001;% -- BFGS Minimizer Line Search Tolerance f --
popedInput.BFGSToleranceg=0.9;% -- BFGS Minimizer Line Search Tolerance g --
popedInput.BFGSTolerancex=0.1;% -- BFGS Minimizer Line Search Tolerance x --
popedInput.ED_samp_size=45;% -- Sample size for ED-optimal design distribution --
popedInput.ED_diff_it=30;% -- Number of iterations in ED-optimal design to calculate convergence criteria --
popedInput.ED_diff_percent=10;% -- ED-optimal design convergence criteria in percent --
popedInput.line_search_it=50;% -- Number of grid points in the line search --
popedInput.iNumSearchIterationsIfNotLineSearch=10;% -- Number of iterations of full Random search and full Stochastic Gradient if line search is not used --

popedInput.parallelSettings.iCompileOption=-1;% -- Compile option for PopED (-1 = No compilation, 0 or 3 = Full compilation, 1 or 4 = Only using MCC (shared lib), 2 or 5 = Only MPI, Option 0,1,2 runs PopED and option 3,4,5 ends after compilation --
popedInput.parallelSettings.iUseParallelMethod=1;% -- Parallel method to use (0 = Matlab PCT, 1 = MPI) --
popedInput.parallelSettings.strAdditionalMCCCompilerDependencies='';% -- Additional dependencies used in MCC compilation (mat-files), if several; space separated --
popedInput.parallelSettings.strExecuteName='calc_fim.exe';% -- Compilation output executable name --
popedInput.parallelSettings.iNumProcesses=2;% -- Number of processes to use when running in parallel (E.g. 3 = 2 workers, 1 job manager) --
popedInput.parallelSettings.iNumChunkDesignEvals=-1;% -- Number of design evaluations that should be evaluated in each process before getting new work from job manager --
popedInput.parallelSettings.strMatFileInputPrefix='parallel_input';% -- The prefix of the input mat file to communicate with the excutable --
popedInput.parallelSettings.strMatFileOutputPrefix='parallel_output';% -- The prefix of the output mat file to communicate with the excutable --
popedInput.parallelSettings.strExtraRunOptions='';% -- Extra options send to e.g. the MPI exectuable or a batch script, see execute_parallel.m for more information and options --
popedInput.parallelSettings.dPollResultTime=1.000000e-01;% -- Polling time to check if the parallel execution is finished --
popedInput.parallelSettings.strFunctionInputName='function_input';% -- The file containing the popedInput structure that should be used to evaluate the designs --
popedInput.parallelSettings.bParallelRS=0;% -- If the random search is going to be executed in parallel --
popedInput.parallelSettings.bParallelSG=0;% -- If the stochastic gradient search is going to be executed in parallel --
popedInput.parallelSettings.bParallelLS=0;% -- If the line search is going to be executed in parallel --
popedInput.parallelSettings.bParallelMFEA=0;% -- If the modified exchange algorithm is going to be executed in parallel --
popedInput.user_data={};% -- User defined data structure that e.g. could be used to send in data to the model --
end
