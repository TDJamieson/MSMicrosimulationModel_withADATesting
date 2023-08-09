#==============================================================================#
#==============================================================================#
#                                                                              #
#---                 Arguments for local PC simulation                      ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

# Some directories differ for cluster processing due to use of a scratch disk  # 
# and also all arguments are passed in to a cluster process, which need to be  #
# directly specified for local simulation.                                     #


  # Run directory location and description --

    runDirectoryDescription <- readline('Please enter run directory description')
    runDescription <- readline('Please enter run description (probably same as above)')
    runsDirectory <- paste0(rootDirectory, 'modelRuns/')
    
  # ---- # 
    
    
  # Define number of cores to use in parallelised processes and set a variable
  # which determines if parallel processes are used
    
    nCores = detectCores()-1
    setDTthreads(nCores)
    
    parallelise = 1 
    
  # ---- #
    
    
  # Initialise variable to be used to 'waste' random nos to avoid
  # repetition of the same numbers
    
    seedRunN = 1
    
  # ---- #
  
      
  # Define timer
    
    time <- Sys.time()
    
  # ---- #
    

#==============================================================================#

