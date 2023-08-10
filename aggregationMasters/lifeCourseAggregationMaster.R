#==============================================================================#
#==============================================================================#
#                                                                              #
#---    Aggregate split populations run through microsimulation process     ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

#  This process takes all of the split microsimulation runs, checks that the   #
#  whole population desired has been simulated, and aggregates them.  This     #
#  will aggregate for each combination of seed group and parameter set ID.     #
#                                                                              #
# ============================================================================ #
                                                                              

#===============================================================================
#                              Load libraries                                  #
#===============================================================================

    # Load libraries

      library(Rcpp)         # --
      library(tidyverse)    # -- General helper functions      
      library(plyr)         # --

      library(openxlsx)     # Reading in from excel
      library(data.table)   # Providing big data type containers for population
                            # --
      library(stringr)      # -- String processing functions
      library(stringi)      # --

#==============================================================================#



#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Tests to see if being run locally or scripted and defines locations and 
  # arguments accordingly using to separate arguments files

    if (interactive() == TRUE){
      
     
      rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting/")
      
      source(paste0(RProjectDirectory, 'aggregationMasters/', 'lifeCourseAggregationMasterArgs_local.R'))
      
      runEnv <- 'local'
      
      print(runEnv)

      
    } else {
     
         
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting/")
      
      source(paste0(RProjectDirectory, 'aggregationMasters/', 'lifeCourseAggregationMasterArgs_cluster.R'))
      
      runEnv <- 'scripted'
      
      print(runEnv)
     
      }

#===============================================================================



#==============================================================================#
#               Define directories to source, and output                       #
#===============================================================================
#                                                                              #
#  Wherever the model runs directory is held, the folder for the run needs     #     
#  to be specified inside this, and the folder in which the split outputs      #
#  are held need to be identified.  An aggregated outputs directory then needs #
#  to be created.  For checking of completion of all runs, the intended        #
#  population needs to be generated from the run controls spreadsheet.  This   #
#  requires a number of further directories and proscesses to be defined.      #
#                                                                              #
# ============================================================================ #                                                
 

  # Define all run non-specific locations
  
    source(paste0(RProjectDirectory, 'universalCode/', 'defineDirectoryLocations.R'))

  # ---- #


  # Specific run directory (needs a 'run directory description' to be defined) --

    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/') 

  # ---- # 
  
    
  # Create aggregated outputs directory --
  
    suppressWarnings(       
        
        dir.create(paste0(runDirectory, 'outputs_aggregated/'))
    
        )
    
    aggregateOutputsDirectory <- paste0(runDirectory, 'outputs_aggregated/')
    
  # ---- #
  
# ==============================================================================
    
    
    
#===============================================================================
#                Define population that should have been run                   #
#===============================================================================
#                                                                              #
# Using openxlsx this loads the control spreadsheet inputs and then uses the   #
# population generation process used to specify the simulation runs to         #
# identify which outputs ought to have been generated.                         #
#                                                                              #
# ============================================================================ #                                                                              

    
  # Load run-specific component inputs
    source(paste0(microsimulationSetupDirectory, 
                  'loadInputs/', "inputs_runControls.R"))
  
    
  # Define some functions of incidental necessity
     source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    
     source(paste0(codeDirectory, 
                  "defineStatisticalFunctions.R"))  
  
    
  # Load universal components which are also of incidental necessity
    source(paste0(microsimulationSetupDirectory, 'loadInputs/', 
                  "inputs_universal.R"))
    
    
  # Generate population 
    source(paste0(microsimulationSetupDirectory, 'createPopulation/', 
                  "specifyPopulations.R"))

# ============================================================================ #

    
    
#==============================================================================#
#                       Identify Population to aggregate                       #
#===============================================================================

  # Identify all seed group and parameter set combinations, then split into
  # the individual runs that were down for each of this to check they
  # have completed

    MSSampleSplits <- as.data.table(do.call(rbind, MSSampleSplits))
    noProfiles <- nrow(unique(MSSampleSplits[, .(seedGroup, parameterSetID)]))

    MSSampleSplits <- split(MSSampleSplits, by = c('seedGroup', 'parameterSetID'))


  # Identify column names for later use

    MSSampleCols <- colnames(MSSampleSplits[[1]])[!grepl('startID|endID|popSize|runningTotalPop',
                                                    colnames(MSSampleSplits[[1]]))]

  
  # If running scripted then set noProfiles to 1 to run only once
   
    if(grepl('scripted', runEnv) == TRUE){

            noProfiles <- 1
  
     }
  
  # Run over all profiles from 1 to number of profiles
 
    for (groupProfile in 1:noProfiles) {
    
  
  # If running scripted then set profile to passed profile
  
  
    if(grepl('scripted', runEnv) == TRUE){
        
      groupProfile <- passedProfile
    
    } 

  # Select population to aggregate from MSSampleSplits and identify profile
 
    popToAggregate <- as.data.table(MSSampleSplits[[groupProfile]])

    seedGroup <- unique(popToAggregate[, seedGroup])

    parameterSetID <- unique(popToAggregate[, parameterSetID])


    writeLines(paste0('Aggregating profile ', groupProfile, ' SG_',seedGroup,
                 '_PSAID_', parameterSetID))

#===============================================================================

    
    
#==============================================================================#
#     Identify output files and compare against population specified           #
#===============================================================================

# NB this checks only the full outputs and assumes if these have completed the 
# other output types will have too.                                            

       
  # Identify output files
  
      outputsDir <- paste0(runDirectory, 'outputs_all/')
      files <- list.files(path = outputsDir)
      files <- files[grepl('to', files)]
      
      print(paste0('Aggregating files in ', outputsDir))
      
  
      files_fullOutputs <- 
         data.table(fileName = 
                      files[grepl('full', files)])
      
      
   # Extract relevant information from file names and store as new fields

      files_fullOutputs[, seedGroup := 
              str_match(fileName, "SG_\\s*(.*?)\\s*_PSAID.*")[,2]]
      files_fullOutputs[, parameterSetID := 
              str_match(fileName, "PSAID_\\s*(.*?)\\s*_person.*")[,2]]
      files_fullOutputs[, startID := 
              str_match(fileName, "personID_\\s*(.*?)\\s*_.*")[,2]]
      files_fullOutputs[, endID := 
              str_match(fileName, "to_\\s*(.*?)\\s*_.*")[,2]]
      files_fullOutputs[, fileName := NULL]
      
      files_fullOutputs[, c(colnames(files_fullOutputs)):=
                                     lapply(.SD, as.integer),
                                     .SDcols = colnames(files_fullOutputs)]
      
 
      
  # Compare with specified population and flag any that are missing
      
      # Keep only common columns
      cols <- intersect(colnames(popToAggregate), colnames(files_fullOutputs))
      aggregationCheck <- popToAggregate[, c(..cols)]
      
      # Identify any missing runs
      setkey(aggregationCheck, seedGroup, parameterSetID, startID, endID)
      setkey(files_fullOutputs, seedGroup, parameterSetID, startID, endID)

      aggregationCheck[, individualLevelRun := FALSE][files_fullOutputs, individualLevelRun := TRUE]

      missingRuns <- aggregationCheck[individualLevelRun == FALSE]
      
  
  # If any missing then save them in a file
  
    if(nrow(missingRuns) > 0){
      write_csv(missingRuns, file = paste0(aggregateOutputsDirectory,
                                           'missingRuns_iter_', groupProfile, 
                                           '_SG_',seedGroup,
                                           '_PSAID_', parameterSetID))
      
       writeLines('Runs Missing - aggregation failed \n')
    
    }


  # Only proceed if the population being aggregated is not present in missing runs
    missingRuns <- missingRuns[seedGroup %in% seedGroup]
    missingRuns <- missingRuns[parameterSetID %in% parameterSetID]

    if (nrow(missingRuns) == 0){
      
      writeLines('Proceeding with aggregation')
      
      
      # Select files specific to this seed group and parameter set 
      
      files <- 
          files[grepl(paste0('SG_', seedGroup), 
                      files)]
        
      files <- 
          files[grepl(paste0('parameterSet_', parameterSetID), 
                      files)]
        
      
      
      # Read in and aggregate different types of output
        outputTypesWanted <- c('full', 'cycleIndependent', 'traces')
        
        lapply(outputTypesWanted, function (output) {
          
          specificFiles <- files[grepl(output, files)]
          
          aggregated <- lapply(specificFiles, function(file) {
              readRDS(paste0(outputsDirectory, file))
            })
          
          aggregated <- as.data.table(do.call(rbind, aggregated))
          
           saveFile(objectToSave = aggregated,
               filename = paste0(output,'_',
                                'SG_', seedGroup,
                                '_PSAID_', parameterSetID,
                                '_', runDescription),
               savePath = paste0(aggregateOutputsDirectory,
                            output,'_',
                            'SG_', seedGroup,
                            '_PSAID_', parameterSetID,
                            '_', runDescription))
          
        })
      
    }
  }
    
    
# ==============================================================================