#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#


#===============================================================================
#                              Load libraries                                  #
#===============================================================================

    # Load libraries

      library(Rcpp)         # --
      library(tidyverse)    # -- General helper functions      
      library(plyr)         # --

      library(openxlsx)     # Reading in from excel
      library(data.table)   # Providing big data type containers for population
      library(doParallel)   # Providing capability for parallelising processes
      
                            # --
      library(stringr)      # -- String processing functions
      library(stringi)      # --
      
      library(dqrng)        # Random numbers - 64-bit general purpose
      library(dirmult)      # Random numbers - dirichlet distributions


#==============================================================================#



#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Tests to see if being run locally or scripted and defines locations and 
  # arguments accordingly using to separate arguments files

    if (interactive() == TRUE){
      
      
      rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting/")
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/', 'generateLifeCoursesMasterArgs_local.R'))
      
    
      } else {
      
      
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting/")
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/', 'generateLifeCoursesMasterArgs_cluster.R'))
      
      
    }
    

#==============================================================================#
    


#===============================================================================
#               Define directories to source, gather, and output               #
#===============================================================================

                                                                      
  # Define all run non-specific locations
  
    source(paste0(RProjectDirectory, 'universalCode/', 'defineDirectoryLocations.R'))

  # ---- #
  

  # Specific run directory (needs a 'run directory description' to be defined) --

    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/') 

  # ---- # 
    
 
  # Output directories --
  
   suppressWarnings(
      dir.create(paste0(runDirectory, 'outputs_all/'))
    )
    outputDirectory <- paste0(runDirectory, 'outputs_all/')
    
  # ---- # 


#==============================================================================#
    

    
#===============================================================================
#                     Define some generic functions                            #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    
    source(paste0(codeDirectory, 
                  "defineStatisticalFunctions.R"))
    
    source(paste0(codeDirectory, 'postMicrosimulationProcesses/',
                  "meltMicrosimulationOutcome.R"))
    
    
    
#==============================================================================#   
    
    

#===============================================================================
#                    Define microsimulation process                            #
#===============================================================================

  source(paste0(microsimulationProcessDirectory, "microsimulationProcess.R"))

#==============================================================================#   
    
    
    
#===============================================================================
#                              Load inputs                                     #
#===============================================================================
#                                                                              #
# Using openxlsx this takes named regions from an excel model spreadsheet and  #
# creates data tables/matrices.                                                #
#                                                                              #
# For transition matrices the from and to states should be in the first column #
# and row of the named range respectively so that the colNames and rowNames    #
# arguments in read.xlsx can pick them up.                                     #
#                                                                              #
# This uses the 'inputValues.xlsx' file that should be held in the model run   #
# run directory specified.                                                     #
#                                                                              #
#------------------------------------------------------------------------------#

  # Run-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_runControls.R"))
    
  # Universal components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_universal.R"))
    
  # Project-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_alemtuzumabADA.R"))
    
  # Sense check inputs
    source(paste0(microsimulationInputProcessesDirectory, "inputs_check.R"))
  
  # Undertake some generic processing of inputs - e.g. PSA parameterisation
    source(paste0(microsimulationInputProcessesDirectory, "inputs_prepare.R"))

#===============================================================================
    
    
    
#==============================================================================#
#                 Identify Populations to run through model                    #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'createPopulation/', 
                  "specifyPopulations.R"))

#===============================================================================
    
    
 
#==============================================================================#
#                 Take population specified by iteration number                #
#===============================================================================

# The 'specifyPopulations' script above returns a data table with a row
# for each seedGroup, and parameter set ID with the number of rows needed
# to satisfy the population size request split into manageable chunks
# specified by 'maxPopSize'

    
    # If running on cluster then take a single MSSample split  to run through
    # microsimulation process
    
    if(interactive() == FALSE){
      
      MSSampleSplits <- list(MSSampleSplits[[profile]])
    
    }
    
    
    for (iteration in 1:length(MSSampleSplits)) {
 
      MSSample <- MSSampleSplits[[iteration]]
      setDT(MSSample)

#-------------------------------------------------------------------------------



#==============================================================================#
#                           Initialise population                              #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'createPopulation/',
                  'initialisePopulation.R'))
      
    print('Population initialised')

#===============================================================================
    
    
    
    
#==============================================================================#
#                   Create outcome random no databases                         #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'generateOutcomeRandomNos.R'))
    
    print('Outcome random numbers generated')

#===============================================================================


#==============================================================================#
#                         Parameterise Population                              #
#===============================================================================
    
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/', 
                  "parameterisePopulation.R"))
    
    print('Population parameterised')

#===============================================================================

    

#==============================================================================#
#       Identify important parameters to include in run description            #
#===============================================================================
    
    inputParameters <- data.table(
      
      startingTreatmentLine = startingTreatmentLine,
      
      startingDMT = ifelse(singleSpecifiedStartingDMT_Switch == 1,
                           includedDMTList[dmtID == singleSpecifiedStartingDMTID,
                                           str_to_title(Name)],
                           'More than one starting DMT'
      ),
      
      
      alemtuzumabADAProportion = unique(MSSample[, alemtuzumabADARisk]),
      
      alemtuzumabEffectiveness = alemtuzumabEffectiveness,
      
      alemtuzumabTestingFNRate = alemtuzumabTestingFNRate,
      
      alemtuzumabTestingFPRate = alemtuzumabTestingFPRate,
      
      
      DMTEffectsActive = ifelse(DMTEffect_Switch == 1,
                                'Yes',
                                'No'),
      
      DMTSwitchingActive = ifelse(DMTSwitching_Switch,
                                  'Yes',
                                  'No'),
      
      DMTSwitchingLevel = ifelse(individualLevelSwitchSequencing_Switch == 1,
                                 'Individual',
                                 ifelse(groupLevelSwitchSequencing_Switch == 1,
                                        'Group',
                                        ifelse(parameterSetLevelSwitchSequencing_Switch == 1,
                                               'Parameter Set', 'Unknown'))),
      
      relapseDistribution = relapseDistribution,
      
      mortalitySet = mortalitySet,
      
      PSA = ifelse(PSA_Switch == 1, 'PSA on', 'PSA off'),
      
      runDescription = runDescription
      
    )
    
    
#===============================================================================
    
    
 
#==============================================================================#
#                          Initialise switching                                #
#===============================================================================

  source(paste0(microsimulationSetupDirectory, "initialiseSwitching.R"))
  
    print('Switching initialised')

#===============================================================================
  
  
  
#==============================================================================#
#                      Run microsimulation process                             #
#===============================================================================

  # Run model and assign outcome
  
  microsimulationOutcomesList <- 
    microsimulationProcess(workingSample = MSSample,
                           outcomeRandNos = outcomeRandNos,
                           DMTRRMSEDSSEffects = DMTRRMSEDSSEffects, 
                           DMTARREffects = DMTARREffects, 
                           DMTSPMSTransitionEffects = DMTSPMSTransitionEffects,
                           RRMSEDSSTransitionMatrix_Rates = transitionMatrix_EDSS_RRMS_Rates, 
                           SPMSEDSSTransitionMatrix = transitionMatrix_EDSS_SPMS,
                           RRMStoSPMSTransitionMatrix = transitionMatrix_RRMStoSPMS)
  
  microsimulationOutcome <- microsimulationOutcomesList[['microsimulationOutcome']]
  
  list2env(microsimulationOutcomesList[['componentOutcomes']], 
           globalenv())
  
  remove(microsimulationOutcomesList, MSSample)
  
#===============================================================================
  
  

#==============================================================================#
#                         Process and save outcomes                            #
#===============================================================================

  print('Saving Outcomes')
  
  # Full unprocessed outcome (split into person and parameter set) ----
    
  
    # Add input parameters
      
      fullOutcome <- cbind(microsimulationOutcome, inputParameters)

  
    # Split by type of individual and parameter set ID
  
      fullOutcomes <- split(fullOutcome, by = c('seedGroup', 'parameterSetID'))
      
      
    # Run over list and save
  
      lapply(fullOutcomes, function(group) {
        
        seedGroup <- unique(group[, seedGroup])
        parameterSetID <- unique(group[, parameterSetID])
        minID <- min(group[, personID])
        maxID <- max(group[, personID])
        
        
        saveRDS(group,
                file = paste0(outputDirectory, runDescription, '__',
                              'SG_', seedGroup, '_',
                              'PSAID_', parameterSetID, '_',
                              'personID_', minID, '_to_',
                              maxID, '_', 'full'))
        })
    
  
      remove(fullOutcomes)
      
      
  # ---- #
      
      
  
  # Cycle independent outcomes (split into person and parameter set) ----
      
    # Select cycle independent fields
    
      cycleIndependentFields <- colnames(fullOutcome)[!grepl('Cycle', colnames(fullOutcome))]
      cycleIndependentOutcomes <- fullOutcome[, c(..cycleIndependentFields)]
      
      
    # Add input parameters
      
      cycleIndependentOutcomes <- cbind(cycleIndependentOutcomes, inputParameters)
      
      
    # Split by type of individual and parameter set ID
      
      cycleIndependentOutcomesList <- 
        split(cycleIndependentOutcomes, by = c('seedGroup', 'parameterSetID'))
      
      
    # Run over list and save
      
      lapply(cycleIndependentOutcomesList, function(group) {
        
        seedGroup <- unique(group[, seedGroup])
        parameterSetID <- unique(group[, parameterSetID])
        minID <- min(group[, personID])
        maxID <- max(group[, personID])
        
        
        saveRDS(group,
                file = paste0(outputDirectory, runDescription, '__',
                              'SG_', seedGroup, '_',
                              'PSAID_', parameterSetID, '_',
                              'personID_', minID, '_to_',
                              maxID, '_', 'cycleIndependent'))
      })
      
      
      remove(BC_TenThousandPop, cycleIndependentOutcomes, cycleIndependentOutcomesList)
      
      
  # ---- #
      
      
  # Long format life courses (split into person and parameter set) ----
      
      
    # Produce long format

      lifeCourseTrace <- 
        wideToLongMicrosimulationOutcome(microsimulationDT = microsimulationOutcome)
      
      
    # Add input parameters
      
      lifeCourseTrace <- cbind(lifeCourseTrace, inputParameters)
      
      
    # Split by type of individual and parameter set ID
      
      lifeCourseTraces <- 
        split(lifeCourseTrace, by = c('seedGroup', 'parameterSetID'))
      
      
    # Run over list and save
      
      lapply(lifeCourseTraces, function(group) {
        
        seedGroup <- unique(group[, seedGroup])
        parameterSetID <- unique(group[, parameterSetID])
        minID <- min(group[, personID])
        maxID <- max(group[, personID])
        
        
        saveRDS(group,
                file = paste0(outputDirectory, runDescription, '__',
                              'SG_', seedGroup, '_',
                              'PSAID_', parameterSetID, '_',
                              'personID_', minID, '_to_',
                              maxID, '_', 'traces'))
      })
      

      remove(outcomeRandNos, DMTRRMSEDSSProgressionEffectsDT,
             microsimulationOutcome,
             seedDT, switchingTables, lifeCourseTrace, lifeCourseTraces)
  
    print('Outcomes saved')
}

    
    
