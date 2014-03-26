qsub -cwd -b y /opt/matlab2009b/bin/matlab -nosplash -nodisplay -nodesktop -r '"poped(function_input())"' -logfile output.log -sge_resource=perf=20
