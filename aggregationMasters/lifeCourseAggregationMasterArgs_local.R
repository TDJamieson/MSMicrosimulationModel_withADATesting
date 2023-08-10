#==============================================================================#
#==============================================================================#
#                                                                              #
#---                 Arguments for local PC simulation                      ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

# Some directories differ for cluster processing due to use of a scratch disk  # 
# and also all arguments are passed in to a cluster process, so need to be     #
# directly specified for local simulation.                                     #


  # Run directory location and description --

    runDirectoryDescription <- readline('Please enter run directory description')
    runDescription <- readline('Please enter run description (probably same as above)')
    runsDirectory <- paste0(rootDirectory, 'modelRuns/')
    
    variation <- readline('Please enter variation (enter "NA" if no variation)')
    
    if(grepl('na', variation, ignore.case = TRUE) == TRUE){
      
       remove(variation)
      
    }
    
  # ---- # 
    
    
#==============================================================================#

