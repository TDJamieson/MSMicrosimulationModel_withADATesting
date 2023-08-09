#==============================================================================#
#==============================================================================#
#                                                                              #
#---         Relapsing-remitting Multiple Sclerosis Markov Model            ---#
#                                                                              #                         
#                          Utility functions                                   #
#                                                                              #
#==============================================================================#  
#==============================================================================#



#==============================================================================#
#  Define a function to check if a file has been saved and re-save if needed   #
#  as large numbers of simultaneous runs may clash accessing disk              #
#===============================================================================


saveFile <- function(filename, objectToSave, savePath){
  
  # Read in files in directory
  
  checkFiles <- list.files(allAggregateOutputsDirectory)
  
  counter <<- 1    
  
  
  # Append '.RData'
  
  filename <- paste0(filename, '.rds')
  
  
  # Save - attempt 1
  
  saveRDS(objectToSave, file = savePath)
  
  
  # If file name is not found in files list then following this then save, and 
  # increment counter - this is allowed to be attempted up to 5 times
  
  while(counter < 5) {
    
    if(length(intersect(checkFiles, filename))==0){
      
      saveRDS(objectToSave, file = savePath)
      
    } 
    
    counter <<- counter + 1
    
  }
  
  counter <<- 1
  
}


#===============================================================================



#==============================================================================#
#  Define a function to run all files in a folder for defining functions etc.  #
#===============================================================================

runAllInFolder <- function(folder) {
  
  files <- list.files(folder, full.names=TRUE)
  
  files <- files[grepl('\\.[rR]$', files, ignore.case = FALSE)]
  
  if(length(files) == 0){
    
    warning(paste0('No files found in ', folder))
    
  } else {
    
    lapply(files, source)
    
  }
  
}

#===============================================================================