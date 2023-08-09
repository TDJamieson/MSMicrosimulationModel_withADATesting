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
# 4. Parallelise dummy - In single core cluster settings trying to parallelise #
#                        is a waste of time and may lead to failure.  This     #
#                        allows parallel processing to be switched on/off.     #
#                                                                              #
# 5. Variation - This optional but allows a variation of a particular input    #
#                variable to passed in for sensitivity analyses.               #
#                                                                              #
#===============================================================================


  # Collect command line arguments
  
    args <- commandArgs(trailing = TRUE)
    
    profile <- as.integer(args[1])
    runDirectoryDescription <- args[2]
    runDescription <- args[3]
    parallelise <- args[4]
    
    print(paste0('Profile: ', profile))
    print(paste0('Run Directory Description: ', runDirectoryDescription))
    print(paste0('Run Description: ', runDescription))
    print(paste0('Parallelise: ', parallelise))
    
    
    if(exists(args[5])){
      variation <- as.numeric(args[5])
      
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