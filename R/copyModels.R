copyModels <- function(models){
  # function to write a line in poped/matlab format  
  print('- Copying models')
  
  # copy files to current model files
  file.copy(models$modfullest, 'runfE.mod', overwrite=TRUE)
  file.copy(models$modfullsim, 'runfS.mod', overwrite=TRUE)
  file.copy(models$modredest , 'runrE.mod', overwrite=TRUE)
  file.copy(models$modredsim , 'runrS.mod', overwrite=TRUE)
}