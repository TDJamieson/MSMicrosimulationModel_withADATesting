#==============================================================================#
#==============================================================================#
#                                                                              #
#---                 Defining the Microsimulation process                   ---#
#                                                                              #
#==============================================================================#  

#==============================================================================#
#                                                                              #
# The model runs on an annual cycle basis, with the maximum number of          #
# cycles being defined in the model spreadsheet.  The cohort is a relapsing    # 
# remitting cohort i.e. all individuals are starting in RRMS.  The processing  #
# keeps together the whole cohort but uses dummy indicators to identify which  #
# state they are in of RRMS and SPMS.  The events that may occur depend on     #
# which state they are in.                                                     #
#                                                                              #
# All individuals, RRMS or SPMS may die in each cycle.  This occurs at the     #
# start of the annual cycle if they do.  All individuals will also age by one  #
# year in each cycle, processed at the end of the cycle.                       #
#                                                                              #
#-                                                                             #
#                                                                              #
# In the RRMS cohort, as well as possibly dying and definitely ageing by one   #
# year the following may also occur:                                           #
#                                                                              #
#  1. Experience relapse(s)                                                    #
#  2. Transition from RRMS to secondary progressive MS (SPMS)                  #
#  3. Move EDSS level, based on RRMS EDSS transition matrix                    #
#                                                                              #
# If the transition to SPMS occurs, there is an inevitable one-step increase   #
# in EDSS and they shift from the RRMS to SPMS cohort, but for the cycle of    #
# transition are not included in the EDSS transition process for the SPMS      #
# group.                                                                       #
#                                                                              #
#-                                                                             #
#                                                                              #
# In addition to dying and definitely ageing by one year, the SPMS cohort      #
# may move EDSS level, as per the SPMS EDSS transition matrix.                 #
#                                                                              #
# ---------------------------------------------------------------------------- #
#                                                                              #
# Starting the process is a data table which contains each individual, their   #
# characteristics, their starting EDSS, DMT and utility and dummies which are  # 
# used to keep track of where individuals are, whether they are in RRMS        #
# disease, SPMS, have died, or have reached age 100.                           #
#                                                                              #
# In each cycle columns are added, regardless of disease state, for RRMS EDSS, #
# SPMS EDSS, and a relapse column, which are filled as appropriate.            #
# If the column is not relevant to that individual, e.g. SPMS individuals      #
# won't experience relapse or have an RRMS EDSS, those individuals have NA     #
# assigned.  Costs and utility columns are created and filled based on         #
# outcomes in the cycle, and only one column for each is created.              #
#                                                                              #
# At the end of the cycle two things happen.  Firstly the outcome columns      #
# from the previous cycle are peeled off and stored in a predefined list       #
# since only current cycle columns e.g. current EDSS, will be used in the next #
# cycle. They are stored with individual IDs for later reunification.          #
# Secondly the set of individuals who have no further processing to be done are#
# identified and their rows removed and stored in a separate predefined list   #
# in order to avoid redundant records slowing processing.  The events that     #
# lead to removal are death and reaching age 100.                              #
#                                                                              #
# This approach means that in each cycle there is a data.table being processed #
# which contains individual IDs, relevant characteristics, enduring dummies,   #
# and last cycle's relevant fields, and as the cycle progresses this cycle's   #
# new fields. At the end of the cycle the cycle outcomes are stored for        #
# each individual, the last cycle's outcomes which had been used in this cycle # 
# are removed and individuals who have completed their trace are removed with  #
# their enduring fields also stored.                                           #
#                                                                              #    
# After the  process is complete, all of the stored outcomes for each          #
# cycle are combined into one data.table by merging on ID which is kept in     #
# every stored item so that the full history of each individual becomes        #
# available in one wrapper.                                                    #
#                                                                              #
#------------------------------------------------------------------------------#


  # ----------------------------------------------------------------------------
  # Define microsimulation process function 
  # ---------     
      
    microsimulationProcess <- 
      function(workingSample, outcomeRandNos, DMTRRMSEDSSEffectsDT, 
               DMTARREffectsDT, DMTSPMSTransitionEffects,
               RRMSEDSSTransitionMatrix_Rates, SPMSEDSSTransitionMatrix,
               RRMStoSPMSTransitionMatrix) {
       
  # -------
   
              
  # -------
       
    # Initialise lists to hold the outcome outputs for each cycle, store      --
    # individuals as they complete processing, and process checklists         --  
      
        outcomesList <- vector(mode = "list", length = nCycles + 1)  # Cycle by cycle outcomes e.g. EDSS in a cycle
        
        outcomesListComplete <- vector(mode = "list", length = nCycles + 1) # Final outcomes e.g. time to SPMS/time to death etc.
        
        mortalityCheckList <- vector(mode = "list", length = nCycles + 1) # Generated to check mortality is behaving as expected
        
        relapseCheckList <- vector(mode = "list", length = nCycles + 1) # Generated to check relapse rates are behaving as expected
        
        RRMStoSPMSCheckList <- vector(mode = "list", length = nCycles + 1) # Generated to check RRMS to SPMS transtion rated are behaving as expected
        
  # -------
        
  
        
  # -------
      
    # Add fields that endure outside of each annual cycle to hold relevant    --
    # outcomes and values that influence processing within cycles             --
    
      
      # Death fields; mark that nobody has yet died using NA                  --
    
        workingSample[, `:=` (Death = NA_integer_,  
                              ageAtDeath = NA_integer_,
                              timeToDeath = NA_integer_,
                              PMLDeath = NA_integer_)]
      
      
      # Fields for reaching age 100                                           --
        
        workingSample[, `:=` (Reached_100 = NA_integer_,
                              timeToReached100 = NA_integer_)]
  
  
      # RRMS or SPMS cohort identifiers (all start in RRMS)                   --
      
        workingSample[, `:=` (RRMSCohort = 1L,
                              SPMSCohort = NA_integer_,
                              secondaryPogressive = NA_integer_)]
        
        workingSample[, timeToSPMS := NA_integer_]
      

      # Field to contain relapse types
      
        workingSample[, relapseTypes := rep(vector(mode = "list", length = 1L), .N)]
        
        
      # Field to follow treatment sequence, a counter for duration            --
      # that the current DMT has been used, and markers for having PML,       --
      # Graves, and Hashimoto's. and the cycle to test in for them            --
    
        workingSample[, `:=` (DMTSequence = paste0(startingDMT, "_0 "),
                              DMTByCycle = paste0("0: ", startingDMT,", "),
                              DMTsWithdrawn = NA_character_,
                              DMTWithdrawalReason = NA_character_,
                              activityAtSwitch = NA_character_,
                              timeOnAnyDMT = NA_integer_,
                              PML = NA_integer_,
                              alemtuzumabThyroidTestingCycle = NA_character_,
                              ATE = NA_integer_,
                              yearsSinceATE = NA_integer_,
                              graves = NA_integer_,
                              hashimotos = NA_integer_)]
        
        
      # Field to mark that an individual has a complete trace and does not    --
      # warrant further processing                                            --
        
        workingSample[, traceComplete := NA_integer_]
        
  # -------
        
        
  # -------    
        
    # Initialise cycle0 fields since they need to contain values for          --
    # processing to be derived from                                           --
        
      # Take values for EDSS in cycle 0 from onsetEDSS field, and set         --
      # SPMS cycle field as NA and relapse indicator for cycle 0 as           --
        
        workingSample[, RRMS_EDSS_Cycle0 := onsetEDSS]
        workingSample[, SPMS_EDSS_Cycle0 := NA_integer_]
        
        workingSample[, Relapses_Cycle0 := 0L]
        workingSample[, lastTwoYearRelapses := 0L]
        
      
      # Set RRMS type - RES or RRMS
       
        workingSample[, RRMSType := 'RRMS']  
  
        
      # Assign initial EDSS (in cycles EDSS may                               -- 
      # be RRMS EDSS or SPMS EDSS) from onset EDSS                            --
     
        workingSample[, EDSS_Cycle0 := RRMS_EDSS_Cycle0]
        
        
      # Mark initial disease-modifying therapy and time on DMT as 1           --
      
        workingSample[, DMT_Cycle0 := startingDMT]
        workingSample[, timeOnCurrentDMT := 1]
        workingSample[!is.na(DMT_Cycle0) & DMT_Cycle0 != withdrawnID, timeOnAnyDMT := 1]
        
        
  # -------
  
        
    
  # -------    
  
      # Add Alemtuzumab project-specific fields
        
        source(paste0(microsimulationProcessDirectory, 'projectSpecificProcesses/',
                      'addAlemtuzumabFields.R'), local = TRUE)
        
  
  # -------    
        
        
  
  # -------    
        
    # Merge in outcome random numbers that are cycle independent              --
    
       workingSample <- 
         workingSample[outcomeRandNos[, c(..singleOutcomeRandomNumberFields, 
                                          'seedGroup', 'personID')],
                     on = .(seedGroup, personID)]
       
  # -------    
       
        
        
  # ---------------------------------------------------------------------------- 
  # Define functions for discrete parts of the annual cycle.
  # NB the environment for these functions needs to be set 
  # ---------   
        
        runAllInFolder(paste0(microsimulationProcessDirectory, 'switchingProcesses/'))
        runAllInFolder(paste0(microsimulationProcessDirectory, 'clinicalOutcomeProcesses/'))

  # ----------------------------------------------------------------------------
  
        
         
  # ----------------------------------------------------------------------------       
  # For loop Which runs over (up to) number of cycles set in                
  # -------
    
      for (cycle in 1:nCycles) {         
 
      # Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
          print(paste0('Cycle:', cycle))
          print(paste0(nrow(workingSample), ' remain'))
      # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

  # -------
           
          assign('cycleNumber', cycle, envir = globalenv())
  
  # ----------------------------------------------------------------------------       
  # Define column names for this cycle and last cycle                          
  # --------
    
    # Identify last and next cycle values                                     --

        lastCycle = cycle - 1L
        nextCycle = cycle + 1L

        
    # Define column names of interest                                         --
        
        # EDSS Values
          
          lastCycleRRMSEDSSCol = paste0("RRMS_EDSS_Cycle", lastCycle)
          currentCycleRRMSEDSSCol = paste0("RRMS_EDSS_Cycle", cycle)
  
          lastCycleSPMSEDSSCol = paste0("SPMS_EDSS_Cycle", lastCycle)
          currentCycleSPMSEDSSCol = paste0("SPMS_EDSS_Cycle", cycle)
  
          lastCycleEDSS <- paste0('EDSS_Cycle', lastCycle)
          currentCycleEDSS <- paste0('EDSS_Cycle', cycle)

        
        # Relapses
          
          lastCycleRelapseCol = paste0("Relapses_Cycle", lastCycle)
          currentCycleRelapseCol = paste0("Relapses_Cycle", cycle)

        
        # DMT
          
          currentCycleDMTCol = paste0("DMT_Cycle", cycle)
          nextCycleDMTCol = paste0("DMT_Cycle", nextCycle)
       
          
        # RRMS Type

          currentCycleRRMSType <- paste0('RRMSType_Cycle', cycle)


 
    # Rename relevant last cycle variables to identify them for use           --
    # in merging/processing etc.                                              --

      if (cycle == 1) {
      
        setnames(workingSample,
               old = c(lastCycleEDSS, 'DMT_Cycle0'),
               new = c('lastCycleEDSS', 'currentCycleDMT'))
        
      } else {
          
      setnames(workingSample,
               old = c(lastCycleEDSS, currentCycleDMTCol),
               new = c('lastCycleEDSS', 'currentCycleDMT'))

      }
      
          
    # Aggregate these fields into last cycle and this cycle                   --

        lastCycleFields <- c(lastCycleRRMSEDSSCol, lastCycleSPMSEDSSCol,
                             lastCycleRelapseCol,
                             'currentCycleDMT', 'lastCycleEDSS')

        
        currentCycleFields <- c(currentCycleSPMSEDSSCol,
                                currentCycleRRMSEDSSCol,
                                currentCycleRelapseCol,
                                nextCycleDMTCol,
                                currentCycleEDSS, 
                                currentCycleRRMSType)
        
        
      
    # Assign new empty columns for this cycle                                 --
      
        workingSample[, c(currentCycleFields) :=  NA_integer_]
        
        # RRMS type needs to be character
        workingSample[, c(currentCycleRRMSType) := NA_character_]
        
        
  # ----------------------------------------------------------------------------
 
 
         
  # ----------------------------------------------------------------------------       
  # Merge in this cycle's random numbers for outcome determination            --
  # ----

    # Select this cycle's random number fields 
     
        cycleRandFields <- outcomeRandomNumberFields[grepl(paste0("_Cycle", cycle, "$"), 
                                                    outcomeRandomNumberFields)]
    
        
   # Remove last cycle's
        
        suppressWarnings(
        workingSample[, c(gsub(paste0("_Cycle", cycle, "$"), "", cycleRandFields))
                      := NULL]
        )
        
        
   # Merge these to working sample
    
        workingSample <- 
         merge(workingSample, 
               outcomeRandNos[, c(..cycleRandFields, 'combinedID')], 
               by = 'combinedID',
               all.x = TRUE)

         
   # Chop off the cycle number in names since it's annoying to incorporate 
   # everywhere          
      
          setnames(workingSample,
                   cycleRandFields,
                   gsub(paste0("_Cycle", cycle, "$"), "", cycleRandFields))

     
   # Now remove from random numbers DT for memory efficiency
        
        outcomeRandNos[, c(cycleRandFields) := NULL]
        
        
  # ----------------------------------------------------------------------------      
  
  
  
  # Assign deaths (whole cohort) ===============================================

      deathAssignment <- assignDeaths(workingSample, cycle)
      
      workingSample <- deathAssignment$population
      mortalityCheckList[[cycle]] <- deathAssignment$mortalityOutcomesDT

      remove(deathAssignment)

 
  # ----------------------------------------------------------------------------

        
      
      
 #  Assign relapses in RRMS group then determine RRMS to SPMS transitions ======

    # Test that there are still members of whole sample that are in RRMS      --

      if (nrow(workingSample[RRMSCohort == 1L & is.na(traceComplete)]) > 0) {
        
         
         # Create subsample of only those in RRMS (and who haven't died)      --
      
            RRMSSubset <- workingSample[RRMSCohort == 1L & 
                                          is.na(traceComplete)]                    
        
            noProgressing <- nrow(RRMSSubset)

            
        # Run relapse assignment process                                      --

            RRMSSubsetRelapseAssignment <- 
              assignRelapses(RRMSPopulation = RRMSSubset, 
                             relapseColumn = currentCycleRelapseCol,
                             cycleNum = cycle,
                             lastCycleNum = lastCycle)
            
            RRMSSubset <- RRMSSubsetRelapseAssignment$RRMSPopulation
            
            relapseCheckList[[cycle]] <- 
              RRMSSubsetRelapseAssignment$relapseOutcomesCheck
        
            remove(RRMSSubsetRelapseAssignment)
            
 
        # Update last two years relapse field to use later in determining     --
        # switching                                                           --
  
            RRMSSubset[, lastTwoYearRelapses := get(lastCycleRelapseCol) + 
                                                 get(currentCycleRelapseCol)]
  
            
            
  #  Determine RRMS to SPMS transitions ----------------------------------------
           
            RRMSSubset <- assignRRMSSPMSTransitions(RRMSSubset, 
                                                    currentCycleSPMSEDSSCol,
                                                    cycle,
                                                    nextCycleDMTCol)
            
  # ----------------------------------------------------------------------------
  
            
                      
  # Outcome checks  ************************************************************
      RRMSToSPMSDT <-
        RRMSSubset[, .(noTransitions = sum(!is.na(get(currentCycleSPMSEDSSCol))),
                     noIndividuals = as.integer(.N)), 
                   by = .(lastCycleEDSS, parameterSetID, 
                          seedGroup, interventionCohort)]
    
      RRMSToSPMSDT[, propTransitioning := noTransitions/noIndividuals,
                    by = .(lastCycleEDSS, parameterSetID, 
                           seedGroup, interventionCohort)]
    
      RRMStoSPMSCheckList[[cycle]] <- RRMSToSPMSDT
    
      remove(RRMSToSPMSDT)
  # ****************************************************************************

  
        # Regenerate workingSample with now processed RRMS cohort             --
            
            workingSample <- rbind(workingSample[RRMSCohort == 0L|
                                                   !is.na(traceComplete)], 
                                   RRMSSubset)
            
            
        # Remove RRMS subset
            
            remove(RRMSSubset)
            
      }

 # =========================================================================== #
 

 
  # Generate RRMS group EDSS transitions =======================================


    # Identify if any remain in RRMS (following above SPMS transitions)       --
          
        if (nrow(workingSample[RRMSCohort == 1L & is.na(traceComplete)]) > 0) {

 
        # Create RRMS Subset
          
          RRMSSubset <- workingSample[RRMSCohort == 1L & 
                                          is.na(traceComplete)]
        
        # Update RRMS Type
        
          RRMSSubset[, c(currentCycleRRMSType) := RRMSType]
          
          
        # Determine transitions on this subset
            
          RRMSSubset <- assignRRMSEDSSTransitions(RRMSPopulation = RRMSSubset,
                                                  RRMSEDSSTransitionMatrix_Rates = RRMSEDSSTransitionMatrix_Rates,
                                                  currentCycleEDSSCol = currentCycleRRMSEDSSCol,
                                                  cycleNum = cycle,
                                                  lastCycleNum = lastCycle)
                      
          
        # Regenerate workingSample with now processed RRMS cohort            --
            
            workingSample <- rbind(workingSample[RRMSCohort == 0L|
                                                   !is.na(traceComplete)], 
                                   RRMSSubset)

       
        # Tidy 
            
          remove(RRMSSubset)
          
          
        } # Close IF
        
  # ============================================================================



  # Generate SPMS group EDSS transitions =======================================

  
    # Test this isn't the first cycle, since there are no SPMS on starting,   --
    # and that there is at least one individual in SPMS                       --
   
      if (cycle != 1 &
          (nrow(workingSample[SPMSCohort == 1L & is.na(traceComplete)
                                     & timeToSPMS != cycle]) > 0)) {

        
      # Identify subset of individuals with SPMS, but not those who have      --
      # transitioned in this cycle, since they have already moved to a higher --
      # EDSS as part of the transition process                                --
        
         SPMSSubset <- workingSample[SPMSCohort == 1L & is.na(traceComplete)
                                     & timeToSPMS != cycle]
            
            
      # Determine transitions on the subset
           
         SPMSSubset <- assignSPMSEDSSTransitions(SPMSSubset,
                                                 currentCycleSPMSEDSSCol)
         
      # Also assign 0 for relapses since SPMS are assumed not to experience   --
      
        SPMSSubset[, c(currentCycleRelapseCol) := 0L]
        
        
      # Regenerate workingSample with now processed RRMS cohort            --
            
          workingSample <- rbind(workingSample[is.na(SPMSCohort) | 
                                                 !is.na(traceComplete) | 
                                                 timeToSPMS == cycle], 
                                 SPMSSubset)
            
      # Tidy
         
         remove(SPMSSubset)

         
      } # Close if

  # ============================================================================




  #  Increment age and mark to exclude individuals over 100 ====================

    
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
                  print('Incrementing age')
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      
    # Increment age of individuals by 1 year (if they have not already
    # been excluded through reaching 100 or dying previously)

        workingSample[currentAge != 100 & is.na(Death),
                      currentAge := currentAge + 1L]


# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  print(paste0(nrow(workingSample[is.na(traceComplete &
                                          currentAge %in% 100)]),
        ' reached 100 this cycle')
        )
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


    # Mark if individual has reached 100 and if so mark as excluded from
    # both RRMS and SPMS subset and mark DMT withdrawal reason

        workingSample[currentAge %in% 100, `:=`
                 (Reached_100 = 1L,
                  traceComplete = 1L,
                  RRMSCohort = 0L,
                  SPMSCohort = 0L,
                  timeToReached100 = cycle,
                  DMTWithdrawalReason = 'Reached 100')]


 # =============================================================================



 # Identify current cycle EDSS state ===========================================

        
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  print('Processing EDSS')
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

        
   # If there are any individuals in RRMS this cycle, who have therefore      --
   # led to an RRMS EDSS column being generated, then produce a field         --
   # with the EDSS for this cycle which is the sum of the SPMS and            --
   # RRMS EDSS columns, though obviously only one column will have a value    --
   # a value for any given person. Na.rm is therefore necessary               --

      if (currentCycleRRMSEDSSCol %in% colnames(workingSample)) {

          workingSample[!is.na(get(currentCycleRRMSEDSSCol)) |
                       !is.na(get(currentCycleSPMSEDSSCol)),

                     eval(currentCycleEDSS) := rowSums(.SD,na.rm = TRUE),

                   .SDcols = c(currentCycleRRMSEDSSCol,
                               currentCycleSPMSEDSSCol)]
        
      } else if (currentCycleSPMSEDSSCol %in% colnames(workingSample)){
        
    
    # Otherwise, just take the SPMS EDSS columnn value, if this has been      --
    # generated, otherwise continue since all have died                                                               --
  
        workingSample[!is.na(get(currentCycleSPMSEDSSCol)),
                     eval(currentCycleEDSS) := get(currentCycleSPMSEDSSCol)]
  
    }
  
 # =============================================================================
 
 
        
 # Assign alemtuzumab autoimmune thyroid disease ===============================

    if (nrow(workingSample[is.na(traceComplete) &
                          alemtuzumabThyroidTestingCycle == cycle]) > 0) {
 
          alemtuzumabForThyroidTestingPopulation <-
            workingSample[is.na(traceComplete) &
                          alemtuzumabThyroidTestingCycle == cycle]


          alemtuzumabForThyroidTestingPopulation <-
            assignAlemtuzumabThyroidDisease(alemtuzumabForThyroidTestingPopulation)

          
          remainingPopulation <-
            workingSample[!is.na(traceComplete) |
                          alemtuzumabThyroidTestingCycle != cycle | 
                          is.na(alemtuzumabThyroidTestingCycle)]


          workingSample <- rbind(alemtuzumabForThyroidTestingPopulation,
                                 remainingPopulation)
    }


 # =============================================================================
 
             
        
 
 # Determine switching  ========================================================
 
                
    # If there are any individuals in RRMS then determine switching if        --
    # DMT switching is turned on in spreadsheet                               --
 
        if (currentCycleRRMSEDSSCol %in% colnames(workingSample) |
            currentCycleSPMSEDSSCol %in% colnames(workingSample)) {

            RRMSPopulation <- 
              workingSample[RRMSCohort == 1L & is.na(traceComplete)]
            
          
          # If switching is active then go through switching processes        --
            
            if (DMTSwitching_Switch  == 1) {
            
              switching <- determineSwitching(
                                  RRMSPopulation = RRMSPopulation,
                                  nextCycleDMTColumn = nextCycleDMTCol,
                                  currentCycleEDSSColumn = currentCycleRRMSEDSSCol,
                                  cycleNum = cycle,
                                  cycleRelapseCol = currentCycleRelapseCol,
                                  switchingTableList = switchingTables
                                )

              
            workingSample <- rbind(switching[['RRMSPopulation']],
                                   workingSample[RRMSCohort == 0L | !is.na(traceComplete)])
            
            switchingTables <- switching[['switchingTables']]
          
              
          # Otherwise if switched off simply withdraw on the basis of EDSS    --
          # or continue current DMT to the next cycle
          
            } else if (DMTSwitching_Switch  == 0) {
              
          
              RRMSPopulation[!is.na(DMTsWithdrawn) | 
                           get(currentCycleRRMSEDSSCol) >= 7,
                        c(nextCycleDMTCol) := withdrawnID]

                
              RRMSPopulation[, c(nextCycleDMTCol) := currentCycleDMT]
           
              
              workingSample <- 
                rbind(RRMSPopulation,
                       workingSample[RRMSCohort == 0L | !is.na(traceComplete)])
              
            }
            

        }
        
      
      # If there is anybody left alive/not reached 100 then assign withdrawn  --
      # for the next cycle to those already withdrawn 
       
        if(nrow(workingSample[is.na(traceComplete)]) > 0) {
          
          workingSample[currentCycleDMT == withdrawnID, c(nextCycleDMTCol) := withdrawnID]
          
        }
            
        
  # ============================================================================
  
    
  # Increment time since adverse event for all who have experienced them =======
    
    if (nrow(workingSample[is.na(traceComplete)]) > 0) {

      workingSample[, `:=` (yearsSinceATE = yearsSinceATE + 1)]
    
    }
        
  # ============================================================================
   
   
   
  # Assign cycle outcomes to lists =============================================

      # Append current cycle outcomes to outcomesList - if all have         ----
      # transitioned to SPMS then append only SPMS field, not RRMS or       ----
      # relapse fields, and if all have been excluded from further          ----
      # processing by way of death or reaching 100 then break               ----
            
          
          # If none remain in RRMS then define fields to include as only    ----
          # current cycle SPMS EDSS, overall EDSS, utilities and costs      ----
          
          if(nrow(workingSample[RRMSCohort == 1L]) == 0 &
             nrow(workingSample[SPMSCohort == 1L]) != 0) {
            
            cycleDT <- workingSample[, c('combinedID', 'interventionID', 
                                         'interventionCohort', 'parameterSetID',
                                         'currentCycleDMT', ..currentCycleFields)]
            
            cycleDT[, c(paste0('MSType_Cycle', cycle)) := 'SPMS']
            
            cycleDT[, c(nextCycleDMTCol, currentCycleRRMSEDSSCol, currentCycleSPMSEDSSCol) := NULL]
            
            setnames(cycleDT, 'currentCycleDMT', paste0('DMT_Cycle', cycle))
              
          }
 
          
          # If all have been excluded from both states then break           ----
            
          if (nrow(workingSample[is.na(traceComplete)]) < 1) {
            
            outcomesListComplete[[cycle]] <- as.data.table(
                                    workingSample[traceComplete == 1L, 
                                                  c('combinedID', 'interventionID', 'seedGroup', 
                                                    'personID', 'parameterSetID', 'currentAge', 'femaleGender',
                                                    'onsetEDSS', 'onsetAge', 'under28Onset', 'sensoryOnset', 
                                                    'Reached_100', 'Death', 'ageAtDeath', 
                                                    'timeToReached100', 'timeToDeath', 
                                                    'timeToSPMS', 'startingDMT', 'startingAge',
                                                    'DMTSequence', 'DMTByCycle', 'activityAtSwitch', 'timeOnAnyDMT',
                                                    'DMTWithdrawalReason', 'interventionCohort',
                                                    'PML', 'graves', 'hashimotos', ..alemtuzumabFields)]
                                  )
            
            break
        
          }
           
            
          # Append current cycle fields to outcomesList and remove the      --
          # fields for the last cycle from working sample for the next      --
          # processing cycle (need to remove next cycle DMT from this and   --
          # rename current cycle DMT to DMT for this cycle for storage)     --
             
          cycleDT <- workingSample[, c('combinedID', 'interventionID', 
                                       'interventionCohort', 'parameterSetID',
                                       'currentCycleDMT', ..currentCycleFields)]
          
          cycleDT[, c(paste0('MSType_Cycle', cycle)) := ifelse(!is.na(get(currentCycleRRMSEDSSCol)), 'RRMS',
                                     ifelse(!is.na(get(currentCycleSPMSEDSSCol)), 'SPMS', 'ERR'))]
          
          cycleDT[, c(nextCycleDMTCol, currentCycleRRMSEDSSCol, currentCycleSPMSEDSSCol) := NULL]
          
          setnames(cycleDT, 'currentCycleDMT', paste0('DMT_Cycle', cycle))
        
          outcomesList[[cycle]] <- cycleDT
          
          remove(cycleDT)
          
          outcomesListComplete[[cycle]] <- as.data.table(
                                    workingSample[traceComplete == 1L, 
                                                  c('combinedID', 'interventionID', 'seedGroup', 
                                                    'personID', 'parameterSetID', 'currentAge', 'femaleGender',
                                                    'onsetEDSS', 'onsetAge', 'under28Onset', 'sensoryOnset', 
                                                    'Reached_100', 'Death', 'ageAtDeath', 
                                                    'timeToReached100', 'timeToDeath', 
                                                    'timeToSPMS', 'startingDMT', 'startingAge',
                                                    'DMTSequence', 'DMTByCycle', 'activityAtSwitch', 'timeOnAnyDMT',
                                                    'DMTWithdrawalReason', 'interventionCohort',
                                                    'PML', 'graves', 'hashimotos', ..alemtuzumabFields)]
                                  )

          
        # Output where people are in processing                               --
        
          completeNo <-
            Reduce(`+`, Filter(Negate(is.null),
                   sapply(1:length(outcomesListComplete),
                  function(x) {nrow(outcomesListComplete[[x]])})))
          
          print(paste0('Trace Complete: ', completeNo))
          
          
          incompleteNo <- nrow(workingSample[is.na(traceComplete)])
          
          print(paste0('Remaining: ', incompleteNo))
          
          
          popSize <- completeNo + incompleteNo
          
          print(paste0('Overall population: ', popSize))
          
        
        
        # Update last cycle fields, remove individuals who have completed     --
        # processing from working sample, and remove last cycle fields        --                                                         --
                  
          lastCycleFields <- lastCycleFields[
            lastCycleFields %in% colnames(workingSample)]
          
          workingSample <- workingSample[is.na(traceComplete)]
          workingSample[, c(lastCycleFields) := NULL]
          
          
          print('END')
      
  # ============================================================================
 
                    
} # Closing brace for cycle 'for' loop
       

 # Produce all final outcomes =================================================

        
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  print(paste0('Preparing final outcomes - elapsed time ', as.integer(difftime(Sys.time(), time)), ' minutes'))
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      # Remove unneeded tables to make space
      
        remove(list = c('switchingTables', 'outcomeRandNos'), inherits = TRUE)
        
   
      # Bind all (non-NULL) elements of the cycled outcomes list and          --
      # separately all the non-NULL elements containing the enduring          --
      # variables, stored for individuals as they exit the population         --
                                
          outcomesListComplete <- Filter(Negate(is.null), outcomesListComplete)
          outcomesListComplete <- rbindlist(outcomesListComplete, fill = TRUE)
          
          outcomesList <- Filter(Negate(is.null), outcomesList)      

          
      # Recreate the original population without the cycled outcomes field    --
      # from the outcomesList which contains the enduring characteristics     --
      # and outcome variables for an individual                               --
          
          workingSample <-
            Reduce(function(d1, d2) 
                    merge(d1, d2, by = c('combinedID', 'interventionID', 
                                         'parameterSetID', 'interventionCohort'), 
                          all.x = TRUE, all.y = FALSE,
                          use.names = TRUE), 
                   outcomesList)
          
          
      # Merge the collapsed list of all outcomes to this on id                --
         
           workingSample <- outcomesListComplete[workingSample,
                                                  on = c('combinedID', 'interventionID', 
                                                 'parameterSetID', 'interventionCohort')]
                   
           
      # Remove NULL cycles from all other lists and collapse to data tables   --
      # to be returned                                                        --
      
           componentOutcomesList <- list(mortalityCheckList = mortalityCheckList, 
                                         relapseCheckList = relapseCheckList,
                                         RRMStoSPMSCheckList = RRMStoSPMSCheckList)
           componentOutcomesListNames <- names(componentOutcomesList)
           
           componentOutcomesList <- 
             lapply(componentOutcomesList, 
                    function(x) {Filter(Negate(is.null), x)
                      rbindlist(x)}
                    )
           
           names(componentOutcomesList) <- gsub("List", "", componentOutcomesListNames)
        
        
      # Tidy                                                                  --
        
          remove(outcomesListComplete)
        
        
      # Return list of outcome lists                                          --

          return(list(microsimulationOutcome = workingSample, 
                      componentOutcomes = componentOutcomesList))
 

}  # Closing brace for microsimulation function

# ========================================================================
 
