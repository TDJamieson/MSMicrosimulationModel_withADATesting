#==============================================================================#
#==============================================================================#
#                                                                              #
#---                Arguments for cluster PC simulation                     ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

# Cluster processing requires a different directory since it uses a scratch    #
# disk and also needs to process arguments passed in rather than directly      #
# specified as with local pc simulation.                                       #
#                                                                              #
# Arguments must be strictly in order and separated by a space in this order:  #
#                                                                              #
# 1. Profile - the row of the 'specified population' to be processed in that   #
#              run.  In Apocrita this will be automatically generated with     #
#              {iter}.                                                         # 
#                                                                              #
# 2. Run Directory Description - the name of the folder in which run-specific  #
#                                inputs are held and outputs will go.          #
#                                                                              #
# 3. Run Description - To describe the run for filename construction           #
#                                                                              #
#                                                                              #
# 5. Variation - This optional but allows a variation of a particular input    #
#                variable to passed in for sensitivity analyses.               #
#                                                                              #
#===============================================================================


  # Collect command line arguments
  
    args <- commandArgs(trailing = TRUE)
    
    passedProfile <- as.integer(args[1])
    runDirectoryDescription <- args[2]
    runDescription <- args[3]

    print(paste0('Profile: ', passedProfile))
    print(paste0('Run Directory Description: ', runDirectoryDescription))
    print(paste0('Run Description: ', runDescription))

    
    if(exists(args[4])){
      
      variation <- as.numeric(args[4])
      print(variation)
      
    }
    
    
  # Define runs directory
    
    runsDirectory <- '/data/scratch/wpw004/'
    
  # ---- #
    
    
  # Define no of cores in case parallel processing is desired
    
    nCores <- as.integer(Sys.getenv('NSLOTS', '1'))
    
  
  # Define timer
  
    time <- Sys.time()
    print(time)
    
 
#==============================================================================#