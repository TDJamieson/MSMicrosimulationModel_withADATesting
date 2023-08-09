#==============================================================================#
#                          Load universal inputs                               #
#===============================================================================
#                                                                              #
#  Using openxlsx this takes named ranges from a spreadsheet which holds the   #
#  universal, project-nonspecific, values needed to run the microsimulation    #
#  such as transition matrices, mortality risk, relapse rates, DMT effects     #
#  etc.                                                                        #
# =============================================================================#



# ------------------------------------------------------------------------------
# Load excel workbook for openxlsx to extract inputs from 
# ----------
  
  # Load spreadsheet
  
    inputValues <- loadWorkbook(paste0(universalInputDirectory, 
                                      "microsimulationInputs.xlsx"))
  
  
  # Define named regions
  
     inputValuesNamedRegions <- openxlsx::getNamedRegions(inputValues)
  
  
# ------------------------------------------------------------------------------
     


# ------------------------------------------------------------------------------
# Load EDSS transition matrices and RRMS to SPMS transition matrix
# ----
     
 # For transition matrices the from and to (i.e. EDSS 0-9) states should be in         
 # the first column and first row of the named range respectively so that     
 # the colNames and rowNames arguments in read.xlsx can pick them up.                        

     
  # Identify transition matrices in excel model - all must be prefixed      --
  # 'transitionMatrix_                                                      --  
    
    transitionMatrixNames <- 
     getNamedRegions(inputValues)[grepl('^transitionMatrix_', 
                                        getNamedRegions(inputValues))]
    
  # ---- #


  # Load transition matrices, name 'From' column, and ensure all            --
  # probabilities are held as numeric                                       --

    transitionMatrices <- lapply(transitionMatrixNames, function(table) {
     
     DT <- as.data.table(
       read.xlsx(inputValues, namedRegion = table,
                 rowNames = FALSE, colNames = TRUE),
       stringsAsFactors = FALSE
     )
     
     setnames(DT, 'X1', 'From')
     
     for (j in colnames(DT))
       set(DT, j=j,
           value = as.numeric(DT[[j]]))
     
     return(DT)
     
    })

  # ---- #



  # Put transition matrices into global environment (need naming first)     --
    
    names(transitionMatrices) <- transitionMatrixNames
    
    list2env(transitionMatrices, globalenv())
    remove(transitionMatrixNames)
  
  # ---- #


  # For RRMS there is split between under and over 28, combine these and   --
  # identify as such                                         --

    transitionMatrix_EDSS_RRMS_over28Onset[, under28 := 0L]
    transitionMatrix_EDSS_RRMS_under28Onset[, under28 := 1L]
    
    
    transitionMatrix_EDSS_RRMS <- 
     rbind(transitionMatrix_EDSS_RRMS_over28Onset,
           transitionMatrix_EDSS_RRMS_under28Onset)
    
    remove(transitionMatrix_EDSS_RRMS_over28Onset,
          transitionMatrix_EDSS_RRMS_under28Onset)
    
  # ---- #



  # For RRMS and SPMS EDSS transition matrices check all rows sum to 1 as   --
  # this will cause errors later if not. RRMS to SPMS transition matrix     --
  # is a special case so does not need testing                              --


  transitionMatrix_EDSS_RRMS[, rowSumsCheck := rowSums(.SD), 
                            .SDcols = c(as.character(0:9))]
  
  transitionMatrix_EDSS_SPMS[, rowSumsCheck := 
                              rowSums(.SD), .SDcols = c(as.character(1:9))]


  if(nrow(transitionMatrix_EDSS_RRMS[abs(rowSumsCheck-1) > 1e-15]) > 0 |
    nrow(transitionMatrix_EDSS_SPMS[abs(rowSumsCheck-1) > 1e-15]) > 0) {
   
   stop('Transition matrix problem - at least one row in EDSS transition
         matrices does not sum to 1.  This will make subsequent
         processing fail.')
  }

  # ---- #

# ------------------------------------------------------------------------------
  
  

# ------------------------------------------------------------------------------
# Load Dirichlet matrices - theese have transition numbers rather than 
# probabilities to allow a dirichlet distribution to be drawn from
# ----

  # Identify transition matrices in excel model - all must be prefixed      --
  # 'dirichlet_transitionMatrix_                                            --
  
    dirichletTransitionMatrixNames <- 
      getNamedRegions(inputValues)[grepl('dirichlet_transitionMatrix_', 
                                         getNamedRegions(inputValues))]
    
  # ---- #
  
  
  # Load transition matrices, name 'From' column, and ensure all numbers    --
  # are held as integer                                                     --
  
    dirichletTransitionMatrices <-
      lapply(dirichletTransitionMatrixNames, 
             
             function(table) {
               
               DT <- as.data.table(
                 read.xlsx(inputValues, namedRegion = table,
                           rowNames = FALSE, colNames = TRUE),
                 stringsAsFactors = FALSE
               )
               
               setnames(DT, 'X1', 'From')
               
               for (j in colnames(DT))
                 set(DT, j=j,
                     value = as.integer(DT[[j]]))
               
               return(DT)
               
             })
  
  # ---- #
  
  
  # Put transition matrices into global environment (need naming first)   --
  
    names(dirichletTransitionMatrices) <- dirichletTransitionMatrixNames
    
    list2env(dirichletTransitionMatrices, globalenv())
    remove(dirichletTransitionMatrices)
    
  # ---- #


# ------------------------------------------------------------------------------
    
    

# ------------------------------------------------------------------------------
# Load regression coefficients used for parameterising distributions to 
# model relapse numbers and all information needed to use regression 
# coefficients
# ----

  # Annual relapse rate                                                     --                                                      
  
    annualRelapseRisk <- as.data.table(
      read.xlsx(inputValues, namedRegion = "relapses_annualRelapseRiskRegression",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    annualRelapseRisk[, Coefficient := str_sub(Coefficient, end = 15)]
    
    annualRelapseRisk[, Coefficient := as.numeric(Coefficient)]
    
    ARRScaleParameter <- 
      annualRelapseRisk[Variable == '(Scale)', Coefficient]
    
  
  # ---- #
  
  
  # Relapse types                                                           --
    
    relapse_subtypes <- as.data.table(
      read.xlsx(inputValues, namedRegion = "relapses_subtypes",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    ) 
  
  # ---- #
  
  
  # Proportion with sensory symptoms at onset of disease                    --
  
    propSensoryOnset <- as.data.table(read.xlsx(inputValues, 
                                                namedRegion = "sensoryOnset_prop",
                                                colNames = TRUE))
    
    propSensoryOnset[, var := varFromProp(Proportion, N)]
    
  
  # ---- #
   
  
  # Age of onset Bands                                                      --
  
    onsetAgeBands <- as.data.table(
      read.xlsx(inputValues, namedRegion = "onsetAgeBands",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )  
  
  # ---- #
  
  
  # Disease duration bands                                                  --
  
    diseaseDurationBands <- as.data.table(
      read.xlsx(inputValues, namedRegion = "diseaseDurationBands",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    ) 
  
  # ---- #

    
# ------------------------------------------------------------------------------
  
    

# ------------------------------------------------------------------------------
# Load DMT information for impact on relapse rates and EDSS progression and 
# ---------
    
  # DMT treatment Effects                                                   --                                                  
    
    DMTARREffects <- as.data.table(
      read.xlsx(inputValues, namedRegion = "DMTARREffects",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    DMTRRMSEDSSProgressionEffects <- as.data.table(
      read.xlsx(inputValues, namedRegion = "DMTRRMSEDSSProgressionEffects",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
  # ----  #   
    
    
  # Remove DMTs not in includedDMTList                                      --
  
    DMTARREffects <- 
      DMTARREffects[dmtID %in% includedDMTList[, dmtID]]
    
    DMTRRMSEDSSProgressionEffects <- 
      DMTRRMSEDSSProgressionEffects[dmtID %in% includedDMTList[, dmtID]]
    
  
  # ---- #
    
    
  # Tidy fields                                                             --
    
    DMTARREffects[, `:=` (dmtID = as.integer(dmtID),
                          Name = tolower(Name))]
    
    
    DMTRRMSEDSSProgressionEffects[, `:=`(dmtID = as.integer(dmtID),
                                         Name  = tolower(Name))]
    
  # ---- #
    
# ------------------------------------------------------------------------------
    
    

# ------------------------------------------------------------------------------
# Add 'complete' DMTs IDs, alemtuzumab dose 2 and 'withdrawn' with relevant
# treatment effects
# --------- 

  # alemtuzumab
    
    DMTARREffects <- rbind(DMTARREffects,
                           DMTARREffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabCompleteID,
                                                Name = 'alemtuzumab complete')])
    
    DMTARREffects <- rbind(DMTARREffects,
                           DMTARREffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabDose2ID,
                                                Name = 'alemtuzumab dose 2')])
    
    DMTARREffects <- rbind(DMTARREffects,
                           DMTARREffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabDose3ID,
                                                Name = 'alemtuzumab dose 3')])
    
    DMTARREffects <- rbind(DMTARREffects,
                           DMTARREffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabCourse1CompleteID,
                                                Name = 'alemtuzumab course 1 complete')])
    
    DMTRRMSEDSSProgressionEffects <- 
      rbind(DMTRRMSEDSSProgressionEffects,
            DMTRRMSEDSSProgressionEffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabCompleteID,
                                                Name = 'alemtuzumab complete')])

    DMTRRMSEDSSProgressionEffects <- 
      rbind(DMTRRMSEDSSProgressionEffects,
           DMTRRMSEDSSProgressionEffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabDose2ID,
                                                Name = 'alemtuzumab dose 2')])
    
    DMTRRMSEDSSProgressionEffects <- 
      rbind(DMTRRMSEDSSProgressionEffects,
            DMTRRMSEDSSProgressionEffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabDose3ID,
                                                Name = 'alemtuzumab dose 3')])
    
    
    DMTRRMSEDSSProgressionEffects <- 
      rbind(DMTRRMSEDSSProgressionEffects,
            DMTRRMSEDSSProgressionEffects[dmtID == alemtuzumabID][,
                                           `:=`(dmtID = alemtuzumabCourse1CompleteID,
                                                Name = 'alemtuzumab course 1 complete')])
    
  # cladribine
    
    DMTARREffects <- rbind(DMTARREffects,
                           DMTARREffects[dmtID == cladribineID][,
                                            `:=`(dmtID = cladribineCompleteID,
                                                 Name = 'cladribine complete')])
    
    DMTRRMSEDSSProgressionEffects <- 
      rbind(DMTRRMSEDSSProgressionEffects,
            DMTRRMSEDSSProgressionEffects[dmtID == cladribineID][,
                                            `:=`(dmtID = cladribineCompleteID,
                                                 Name = 'cladribine complete')])
    
  # Withdrawn
    
    DMTARREffects <- rbind(DMTARREffects, list(dmtID = withdrawnID, 
                                               Name = 'withdrawn',
                                               IRR = 1,
                                               SEM = 0,
                                               CI_Lower = 1,
                                               CI_Higher = 1))
    
  
    DMTRRMSEDSSProgressionEffects <- rbind(DMTRRMSEDSSProgressionEffects, list(dmtID = withdrawnID, 
                                                                               Name = 'withdrawn',
                                                                               RR = 1,
                                                                               SEM = 0,
                                                                               CI_Lower = 1,
                                                                               CI_Higher = 1))
    
    # ----
    
# ------------------------------------------------------------------------------
  
   
     
# ------------------------------------------------------------------------------
# Load DMT information for general intolerance and specific adverse events - 
# autoimmune thyroid events for alemtuzumab and progressive multifocal 
# leukoencephalopathy for natalizumab (arising from JCV reactivation)
# ---------  
    
    
  # PML on natalizumab ----
    
  # Anti-JCV prevalence                                                     --
  
    antiJCVPrevalence <- as.data.table(
      read.xlsx(inputValues, namedRegion = "antiJCVPrevalence",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    antiJCVPrevalence[, meanAsProp := get(paste0('Mean(%)'))/100]
  
  # PML Risk                                                                --
    
    PMLRisk <- as.data.table(
      read.xlsx(inputValues, namedRegion = "PMLRisk",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    setnames(PMLRisk,
             old = c("Years.of.Natalizumab", "PML.Risk", 
                     "Anti-JCV.Seropositivity"), 
             new = c('natalizumabDuration', 'PMLRisk', 'seropositivity'))
    
    
  # PML mortality Risk                                                     -- 
  
    pmlMortalityRisk <- as.numeric(read.xlsx(inputValues, 
                                             namedRegion = "pmlMortalityRisk",
                                             colNames = FALSE)) 
  
  # ---- #
  
  
  # alemtuzumab autoimmune thyroid disease risk                             --
  
    alemtuzumabThyroidDisease <- as.data.table(
      read.xlsx(inputValues, namedRegion = "adverseEvents_alemtuzumabThyroidDisease",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
  
  # ---- #
  
  
  
  # All DMT Intolerance Risks                                               --
  
    DMTIntoleranceRisks <- as.data.table(
      read.xlsx(inputValues, namedRegion = "DMTIntoleranceRisks",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    setnames(DMTIntoleranceRisks, 
             'Intolerance.Rate', 
             'rate_Intolerance')
    
  
  # Add 'complete' and 'dose 2' risk for alemtuzumab because of the 
  # availability of the third dose
  
    DMTIntoleranceRisks <- 
      rbind(DMTIntoleranceRisks,
            DMTIntoleranceRisks[dmtID == alemtuzumabID][,
                                 dmtID := alemtuzumabCompleteID])
    
    
    DMTIntoleranceRisks <- 
      rbind(DMTIntoleranceRisks,
            DMTIntoleranceRisks[dmtID == alemtuzumabID][,
                                 dmtID := alemtuzumabDose2ID])
  
  # ---- #
  
  
  # Tidy
  
    DMTIntoleranceRisks <- 
      DMTIntoleranceRisks[, .(dmtID, Name, rate_Intolerance, Var)]
    
  
  # ---- #
    

# ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# Read in mortality information
# ---- 

  # Life tables by gender - N.B using qx (probability of individual age x   --
  # dying before age x+1, as defined in ONS spreadsheet and explained in    --
  # excel model)                                                            --

    maleLifeTable <- as.data.table(
      read.xlsx(inputValues, namedRegion = "maleLifeTable",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    maleLifeTable[, `:=`(femGender = 0L,
                         mortalityRisk = qx,
                         qx = NULL,
                         mx = NULL)]
    
    
    
    femaleLifeTable <- as.data.table(
      read.xlsx(inputValues, namedRegion = "femaleLifeTable",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    femaleLifeTable[, `:=`(femGender = 1L,
                           mortalityRisk = qx,
                           qx = NULL,
                           mx = NULL)]
    
    lifeTable <- rbindlist(list(maleLifeTable, femaleLifeTable))
    
    remove(maleLifeTable, femaleLifeTable)
    
    
  # EDSS-specific mortality multipliers (there are two mortality sets which --
  # can be used - the choice is set in the controls spreadsheet)            --
    
    if(mortalitySet == 'Zimmermann'){
      
      EDSSMortalityMultipliers <- as.data.table(
        read.xlsx(inputValues, namedRegion = "EDSSMortalityMultipliers_Zimmermann",
                  rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
      )
    }
    
    
    
    if(mortalitySet == 'Harding'){
      
      EDSSMortalityMultipliers <- as.data.table(
        read.xlsx(inputValues, namedRegion = "EDSSMortalityMultipliers_Harding",
                  rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
      )
    }

# ------------------------------------------------------------------------------
    
    
    
# ----------------------------------------------------------------------------
# Create DT with all combinations of all characteristics to reproducibly
# ID groups - 'seedGroup' across populations
# -----------

    seedDT <- expand_grid(onsetAge = c(0:100),
                          currentAge = c(0:100),
                          femaleGender = c(0,1),
                          onsetEDSS = c(0:9))
    
    setDT(seedDT)


  # Group ID is simply row ID  for unique characteristic set in DT

    seedDT[, seedGroup := .I]


# ----------------------------------------------------------------------------
    
    
     
# ----------------------------------------------------------------------------
# Tidy all DMT tables so that drug names are lower and dmtID is integer for
# all tables containing DMT information
# ----
  
  # Get list of DMT tables, by searching for objects named 'DMT', excluding
  # _Switch for 1/0 control switches and 'singleSpecifiedStartingDMTID' which
  # is not a table

    DMTTables <- ls(pattern = 'DMT')
    DMTTables <- DMTTables[!grepl('_Switch', DMTTables)]
    DMTTables <- DMTTables[!grepl('_probabilisticSwitch', DMTTables)]
    DMTTables <- DMTTables[!grepl('singleSpecifiedStartingDMTID', DMTTables)]
    DMTTables <- DMTTables[!grepl('seedRun_', DMTTables)]
    
    DMTTables <- mget(DMTTables)
    
    DMTTables <- lapply(DMTTables, function(table) {
      table[, `:=`(dmtID = as.integer(dmtID),
                   Name = tolower(Name))]
    })
    
    switchingTables <- lapply(switchingTables, function(table) {
      table[, `:=`(dmtID = as.integer(dmtID),
                   Name = tolower(Name))]
    })
    
    allDMTTables <- append(DMTTables, switchingTables)


  # Tidy 
  
    remove(DMTTables)

# ----------------------------------------------------------------------------     
    
