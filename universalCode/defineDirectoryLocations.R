#==============================================================================#
#==============================================================================#
#                                                                              #
#---                 Define universal folder locations                      ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#


# =============================================================================#
#                                                                              #
# There need to be four directories defined - the location of the R project,   #
# which has already been defined in the master file, the location of run       #
# non-specific inputs (essentially life course determining inputs), a          #
# run folder in which the run-specific inputs are held, and then inside this   #
# an outputs folder where run outputs are stored.                              #
#                                                                              #
# The R project location has been defined in the master R file and the run     #
# directory will also subsequent to this be defined in the master file.  This  #
# defines where microsimulation code and universal inputs are to be found.     #                                                                             
#                                                                              #
# ==============================================================================                                                                                

  # R process directories --
  
    codeDirectory <- paste0(RProjectDirectory, "microsimulationCode/")

    
    # Input loading and parameterisation processes folder
    microsimulationSetupDirectory <-                
      paste0(codeDirectory, 'microsimulationSetup/')
    
    
    # Input processes folder
    microsimulationInputProcessesDirectory <- 
      paste0(microsimulationSetupDirectory, 'loadInputs/')
    
    
     # Parameterisation processes folder
    microsimulationParameterisationProcessesDirectory <- 
      paste0(microsimulationSetupDirectory, 'parameterisePopulation/')
    
    
    # Folder containing all microsimulation process file folders
    microsimulationProcessDirectory <- 
      paste0(codeDirectory, 'microsimulationProcess/')
    
    
    # Switching processes subdirectory of microsimulation process folder
    switchingDirectory <- paste0(codeDirectory, 'microsimulationProcessFiles/switchingProcesses/')
    postProcessingDirectory <- paste0(codeDirectory, 'postMicrosimulationProcessing/')

  # ---- # 

    
  # Universal inputs directory --
    
    universalInputDirectory <- paste0(rootDirectory, 'inputs_microsimulation/')

  # ---- # 

    
# ============================================================================ #