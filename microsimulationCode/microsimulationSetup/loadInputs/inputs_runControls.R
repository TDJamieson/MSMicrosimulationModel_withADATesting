#==============================================================================#
#                          Load universal inputs                               #
#===============================================================================
#                                                                              #
#  Using openxlsx this takes named ranges from a spreadsheet which holds the   #
#  options to determine how a specific run of the micosimulation model is      #
#  set up.                                                                     #
#                                                                              #
# =============================================================================#



# ------------------------------------------------------------------------------
# Load excel workbook for openxlsx to extract inputs from 
# ----------

  # Load spreadsheet
  
    modelControls <- loadWorkbook(paste0(runDirectory, 
                                         "modelControls.xlsx"))
  
  
  # Define named regions
  
    modelControlsNamedRegions <- openxlsx::getNamedRegions(modelControls)
    

# ------------------------------------------------------------------------------#
    
    
    
# ------------------------------------------------------------------------------      
# Load all 'switch' values
# ----------
    
  # Load all general switches, must be identified by '_Switch' suffix     --
    
    switchFields <- modelControlsNamedRegions[grepl("_Switch", 
                                                    modelControlsNamedRegions)]
    
    switches <- lapply(switchFields, 
                       function(switch) {
                         as.integer(          
                           
                           read.xlsx(modelControls, namedRegion = switch, colNames = FALSE)
                           
                         )
                         
                       })
    
    names(switches) <- switchFields
    
    list2env(switches, globalenv())
    
    
  # ---- #
    
    
  # Load all PSA switches, identified by '_probabilisticSwitch' suffix    --
    
    probabilisticSwitchFields <- 
      modelControlsNamedRegions[grepl("_probabilisticSwitch", 
                                      modelControlsNamedRegions)]
    
    probabilisticSwitches <- 
      lapply(probabilisticSwitchFields, function(name) {
        as.integer(
          read.xlsx(modelControls, namedRegion = name, colNames = FALSE)
        )
      })
    
    names(probabilisticSwitches) <- probabilisticSwitchFields
    
  # ---- ~
    
    
  # Define range to use for seeding with PSA if at parameter set          --
    
    PSASeedRangeMin <- as.integer(read.xlsx(modelControls, 
                                            namedRegion = "PSA_parameterSetRangeMin", 
                                            colNames = FALSE))
    
    PSASeedRangeMax <- as.integer(read.xlsx(modelControls, 
                                            namedRegion = "PSA_parameterSetRangeMax", 
                                            colNames = FALSE))
    
    PSASeedRange <- seq(PSASeedRangeMin, PSASeedRangeMax)
    
    
  # ----
    
    
    
  # There are two high level switches for PSA - one to turn it on and off 
  # entirely, and one to set all of the PSA switches to 1, i.e. on.  The 
  # values of these may override switch values
    
    
  # First if fully probabilistic switch is 1 set all individual switches  --
  # to 1
  
    if (fullyProbabilistic_Switch == 1) {
      probabilisticSwitches[] <- 1L
    }
    
    
  # Now override this if PSA is switched off, otherwise the PSA switch
  # values will be carried through   
    
  # Turn off PSA if set at 0
    
    if (PSA_Switch == 0) {
      
      probabilisticSwitches[] <- 0L
      PSASeedRange <- 1
      
    }
    
    list2env(probabilisticSwitches, globalenv())
    
  # ---- #
    
    
  # Get starting personID so that e.g. person ID is 500,000 - 1,000,000   --
    
    startingPersonID <- as.integer(read.xlsx(modelControls, 
                                             namedRegion = "startingPersonID", 
                                             colNames = FALSE))
    
  # ---- 
    
    
  # Distribution to use for relapse modelling - both Poisson and          --
  # negative binomial have been coded to be avialable as distributions    --
  # so this choice can be made in the spreadsheet                         --
  
    relapseDistribution <- as.character(read.xlsx(modelControls, 
                                                  namedRegion = "relapseDistribution", 
                                                  colNames = FALSE))
    
  
  # ---- #
    
    
  # Set of mortality values to use for EDSS-specific mortality multipliers--
    
    mortalitySet <- as.character(read.xlsx(modelControls, 
                                           namedRegion = "mortalitySet", 
                                           colNames = FALSE))  
    
  # ---- #
    
    
  # Starting DMT Treatment Line - may be 1st, 2nd etc.                    --
    
    startingTreatmentLine <- as.integer(read.xlsx(modelControls, 
                                                  namedRegion = "startingTreatmentLine", 
                                                  colNames = FALSE))
    
  # ---- #
    
    
    
  # Tidy                                                                    --
  
  remove(switches, switchFields, 
         probabilisticSwitches, probabilisticSwitchFields)
  
  # ---- #
    
# ------------------------------------------------------------------------------
  
  

# ------------------------------------------------------------------------------
# Population features
# ----

  # Maximum population size to run through the model to avoid memory      --
  # memory overflow                                                       --

    maxPopSize <- 
      as.integer(read.xlsx(modelControls, 
                           namedRegion = "maxPopSize", 
                           colNames = FALSE))


  # Number of repeats per type of individual

    noReplicants <- 
      as.integer(read.xlsx(modelControls, 
                           namedRegion = "noReplicants", 
                           colNames = FALSE))

  # ---- #


  # Grid holding sample population characteristics desired. NB some ranges
  # are included in that grid so that there is a two stage expansion
  # process to create a population.  Firstly a set of single individuals 
  # are created taking each row with each value in each range expanded to 
  # give a set of individuals with unique characteristics.  These 
  # unique individuals are then replicated as many times as is desired 
  # for the modelling process, as defined by 'noReplicants'

    preSpecifiedPopulation <- 
      as.data.table(
        read.xlsx(modelControls, namedRegion = "preSpecifiedPopulationGrid",
                  rowNames = FALSE, colNames = TRUE)
      )
    
    preSpecifiedPopulation[, Starting.DMT.Name := NULL]


  # ---- #



  # ID of DMT all are starting on if a specified single starting DMT      --
  # has been chosen                                                       --
  
    singleSpecifiedStartingDMTID <- 
      as.integer(read.xlsx(modelControls,
                           namedRegion = "singleSpecifiedStartingDMTID",
                           colNames = FALSE))
  
  # ---- #

  
# ------------------------------------------------------------------------------
  
  
  
# ----------------------------------------------------------------------------
# Determine max number of cycles potentially needed to pre-specify number of 
# random numbers needed to reduce waste
# ----

    nCycles <- max(100 - preSpecifiedPopulation[which.min(MinOnsetAge), 
                                                MinOnsetAge],
                   preSpecifiedPopulation[which.min(MinAge), MinAge])

# ----------------------------------------------------------------------------
  
  
  
# ------------------------------------------------------------------------------
# DMT information
# ----        

  # Included DMTs                                                           --                                                         
  
    includedDMTList <- as.data.table(
      read.xlsx(modelControls, namedRegion = "includedDMTList",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )

  
  # Get list of switching tables in spreadsheet                           --
    
    switchTableNames <- 
      getNamedRegions(modelControls)[grepl('^switching_', 
                                           getNamedRegions(modelControls))]
  
  
  
  
  # Read switching tables in (and exclude DMTs not in included list)      --
  
    switchingTables <- lapply(switchTableNames, function(table) {
      
      DT <- as.data.table(
        read.xlsx(modelControls, namedRegion = table,
                  rowNames = FALSE, colNames = TRUE),
        stringsAsFactors = FALSE
      )
      
      DT <- DT[dmtID %in% includedDMTList[, dmtID]]
      
    })

  
  # Load linked DMT list which is used for excluding a DMT included       --
  # at a different dose or which is in the same class                     --
  
    linkedDMTs <- as.data.table(
      read.xlsx(modelControls, namedRegion = 'linkedDMTs',
                rowNames = FALSE, colNames = TRUE),
      stringsAsFactors = FALSE
    )
  
  # ---- #
  
# ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# Identify DMT IDs for those that need to be specifically identified
# for idiosyncratic processing (both ARR and EDSS progression table)
# and assign with DMT effect for that DMT
# ---------
    
    # alemtuzumab
    
    alemtuzumabID <- includedDMTList[grepl('alemtuzumab', Name,
                                           ignore.case = TRUE), dmtID]
    
    alemtuzumabCompleteID <- as.integer(paste0(alemtuzumabID,'00'))
    
    alemtuzumabDose2ID <- as.integer(paste0(alemtuzumabID,'02'))
    
    alemtuzumabDose3ID <- as.integer(paste0(alemtuzumabID,'03'))
    
    alemtuzumabCourse1CompleteID <- as.integer(paste0(alemtuzumabID,'021'))
    
    
    # cladribine
    
    cladribineID <- includedDMTList[grepl('cladribine 3.5mg', Name,
                                          ignore.case = TRUE), dmtID]
    cladribineCompleteID <- as.integer(paste0(cladribineID,'00'))
    
    
    natalizumabID <- includedDMTList[grepl('natalizumab', Name,
                                           ignore.case = TRUE), dmtID]
    
    withdrawnID <- as.integer(99)
    
    # ---- #
    
# ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# Add these to includedDMTList
# ---------
    
    # alemtuzumab
    
      includedDMTList <- rbind(includedDMTList,
                             includedDMTList[dmtID == alemtuzumabID][,
                                               `:=`(dmtID = alemtuzumabCompleteID,
                                                    Name = 'alemtuzumab complete')])
      
      includedDMTList <- rbind(includedDMTList,
                             includedDMTList[dmtID == alemtuzumabID][,
                                               `:=`(dmtID = alemtuzumabDose2ID,
                                                    Name = 'alemtuzumab dose 2')])
      
      includedDMTList <- rbind(includedDMTList,
                             includedDMTList[dmtID == alemtuzumabID][,
                                               `:=`(dmtID = alemtuzumabDose3ID,
                                                    Name = 'alemtuzumab dose 3')])
      
      includedDMTList <- rbind(includedDMTList,
                           includedDMTList[dmtID == alemtuzumabID][,
                                             `:=`(dmtID = alemtuzumabCourse1CompleteID,
                                                  Name = 'alemtuzumab course 1 complete')])
    
    # cladribine
    
      includedDMTList <- rbind(includedDMTList,
                               includedDMTList[dmtID == cladribineID][,
                                                `:=`(dmtID = cladribineCompleteID,
                                                     Name = 'cladribine complete')])
    
    # cladribine
    
      includedDMTList <- rbind(includedDMTList, list(dmtID = withdrawnID, 
                                                     Name = 'withdrawn'))
     
# ------------------------------------------------------------------------------
    
 
# ------------------------------------------------------------------------------
# Add the 'complete' ids for alemtuzumab and cladribine and dose 2 for  
# alemtuzumab  
# --------

    linkedDMTs <- rbind(linkedDMTs, 
                        list(dmtID = alemtuzumabCompleteID, 
                             Name = 'alemtuzumab',
                             linkedDMTID = alemtuzumabID, 
                             linkedDMTName = 'alemtuzumab'),
                        list(dmtID = alemtuzumabDose2ID, 
                             Name = 'alemtuzumab dose 2',
                             linkedDMTID = alemtuzumabID, 
                             linkedDMTName = 'alemtuzumab'),
                        list(dmtID = alemtuzumabDose3ID, 
                             Name = 'alemtuzumab dose 3',
                             linkedDMTID = alemtuzumabID, 
                             linkedDMTName = 'alemtuzumab'),
                        list(dmtID = alemtuzumabCourse1CompleteID, 
                             Name = 'alemtuzumab course 1 complete',
                             linkedDMTID = alemtuzumabID, 
                             linkedDMTName = 'alemtuzumab'),
                        list (dmtID = cladribineCompleteID, 
                              Name = 'cladribine Complete',
                              linkedDMTID = cladribineID, 
                              linkedDMTName = 'cladribine'))
    
    setDT(linkedDMTs)
  
  # ---- #
  
  
  # Tidy and make drug names lowercase and dmtIDs integer                 --
  
    linkedDMTs[, `:=`(dmtID = as.integer(dmtID),
                      linkedDMTID = as.integer(linkedDMTID),
                      linkedDMTName = tolower(linkedDMTName),
                      Name = tolower(Name))]
  
  
  # Name tables and tidy
  
    names(switchingTables) <- gsub('switching_', "", switchTableNames)
    
    remove(switchTableNames)
  
  # ---- #
  
  
# ------------------------------------------------------------------------------
    
    

    
# ------------------------------------------------------------------------------
# Read in and split matrix of seed increments for supporting reproducibility 
# of random number generation
# -----------

    seedMatrix <- read.xlsx(modelControls, namedRegion = "seedMatrix",
                            rowNames = TRUE, colNames = TRUE)
    
    seedMatrixVals <- as.list(seedMatrix$seedValue)
    names(seedMatrixVals) <- rownames(seedMatrix)
    
    list2env(seedMatrixVals, envir = globalenv())
    
    seedRunN <- as.integer(read.xlsx(modelControls, namedRegion = "seedRunN",
                                     rowNames = FALSE, colNames = FALSE))


# ------------------------------------------------------------------------------
