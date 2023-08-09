#==============================================================================#
#                                                                              #
#---                         DMT Switching process                          ---#
#                                                                              #
#==============================================================================#  

# This takes the population from the cycling cohort who have been identified   #
# as being in RRMS and determines if they need to be switched or withdrawn on  #
# the basis of clinical activity or intolerance.  It splits the population     #
# into relevant 


determineSwitching <- function(RRMSPopulation = workingSample,
                               nextCycleDMTColumn = nextCycleDMTCol,
                               currentCycleEDSSColumn = currentCycleEDSSCol,
                               cycleNum = cycle,
                               cycleRelapseCol = currentCycleRelapseCol,
                               switchingTableList = switchingTables) {
 
    # Produce population size check                                           --
        
        RRMSPopulationSize <- nrow(RRMSPopulation)
        
        
    # Create outcome fields                                                   --
     
        RRMSPopulation[, `:=` (DMTIntolerance = NA_integer_,
                               switch = NA_integer_,
                               clinicalActivity = NA_integer_)]
        
    
   
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  print('Determining switching')
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 
    # --------------------------------------------------------------------------
    # Mark if previously in RES (done first for later convenience)
    # ----      
    
          RRMSPopulation[RRMSType == 'RES', existingRES := 1L]
          RRMSPopulation[RRMSType == 'RRMS', existingRES := 0L]

        
    # --------------------------------------------------------------------------
    
          
       
    # --------------------------------------------------------------------------
    # Create empty DT for when population does not exist but something
    # needs to exist for rbind
    # -------
    
          emptyDT <- RRMSPopulation[0]
               
    # --------------------------------------------------------------------------
          
          
              
    # --------------------------------------------------------------------------
    # Identify those who have already had DMTs withdrawn and assign 
    # withdrawn again for next cycle - split remaining RRMS population
    # ----    
      
        previouslyWithdrawnPopulation <- RRMSPopulation[!is.na(DMTsWithdrawn)]
        
        previouslyWithdrawnPopulation[, c(nextCycleDMTColumn) := withdrawnID]
        
        RRMSPopulation <- RRMSPopulation[is.na(DMTsWithdrawn)]
        
  
    # ----
    
        
        
    # --------------------------------------------------------------------------
    # Identify those who have an EDSS >= 7 and assign withdrawn
    # ----  
    
        EDSSOver7Population <- RRMSPopulation[get(currentCycleEDSSColumn) >= 7]
        
        EDSSOver7Population[, c(nextCycleDMTColumn) := withdrawnID]
                              
          
        RRMSPopulation <- RRMSPopulation[get(currentCycleEDSSColumn) < 7] 
      
      
      # Also mark reason for withdrawal
      
        EDSSOver7Population[, DMTWithdrawalReason := "Reached EDSS 7"]
      

    # -------------------------------------------------------------------------
          

           
    # --------------------------------------------------------------------------
    # Determine clinical activity for remaining population to be used in 
    # switching decision
    # ---------------      
          
          RRMSPopulation <- 
          determineClinicalActivity(population = RRMSPopulation,
                                    currentCycleRelapseCol = cycleRelapseCol)
          
          
    # -------------------------------------------------------------------------

          
 
    # --------------------------------------------------------------------------
    # Split populations by DMT since some vary in the way they are 
    # administered, monitored, and in their adverse events 
    # (Check numbers are adding up after splitting)
    # ---------------
    
      # Split alemtuzumab population off
      
        alemtuzumabPopulation <- RRMSPopulation[currentCycleDMT == alemtuzumabID | 
                                                currentCycleDMT == alemtuzumabCompleteID |
                                                currentCycleDMT == alemtuzumabCourse1CompleteID | 
                                                currentCycleDMT == alemtuzumabDose2ID]
        
        RRMSPopulation <- RRMSPopulation[currentCycleDMT != alemtuzumabID &
                                         currentCycleDMT != alemtuzumabCompleteID &
                                         currentCycleDMT != alemtuzumabCourse1CompleteID & 
                                         currentCycleDMT != alemtuzumabDose2ID]
        
        
      # Split cladribine population off
      
        cladribinePopulation <- RRMSPopulation[currentCycleDMT == cladribineID |
                                               currentCycleDMT == cladribineCompleteID]
        
        RRMSPopulation <- RRMSPopulation[currentCycleDMT != cladribineID & 
                                         currentCycleDMT != cladribineCompleteID]
        
        
      # Split natalizumab population off
      
        natalizumabPopulation <- RRMSPopulation[currentCycleDMT == natalizumabID]
        
        RRMSPopulation <- RRMSPopulation[currentCycleDMT != natalizumabID]
      
        
      # Take remainder
      
        otherDMTPopulation <- RRMSPopulation
        
        remove(RRMSPopulation)
        

      # Check 
      
        checkNum <- Reduce(`+`, 
                          sapply(ls(pattern = '.*Population$'), 
                                 function(DT) {
                                   nrow(get(DT))
                                   })
                          )
        
        if(checkNum != RRMSPopulationSize){
          
          stop(' !!! Population splitting has either duplicated or lost people !!! ')
        
          }
        
      
    # --------------------------------------------------------------------------
    
 
            
    # --------------------------------------------------------------------------
    # Process cladribine population
    # ----   
        
        if (nrow(cladribinePopulation) > 0) {
          
           cladribinePopulation <- cladribineProcessing(population = cladribinePopulation,
                                                        nextCycleDMTColname = c(nextCycleDMTColumn),
                                                        cycleNum = cycleNum)
           
           cladribineSwitch_intolerance <- cladribinePopulation$cladribineSwitch_intolerance
           cladribineSwitch_clinicalActivity <- cladribinePopulation$cladribineSwitch_clinicalActivity
           cladribine_continuing <- cladribinePopulation$cladribine_continuing
          
           remove(cladribinePopulation)
           
        } else { # If nobody on cladribine create blank DTs for later rbind
          
          cladribineSwitch_intolerance <- emptyDT
          cladribineSwitch_clinicalActivity <- emptyDT
          cladribine_continuing <- emptyDT
          
        }
            
    
    # --------------------------------------------------------------------------    
    
        
            
    # --------------------------------------------------------------------------
    # Process alemtuzumab population
    # ---- 
 
      # Check if any are on alemtuzumab then run processing
        
        if (nrow(alemtuzumabPopulation) >0) {

          
        # Note anybody who has had alemtuzumab
          
          alemtuzumabPopulation[, alemtuzumabTreated := 1L]
          
     
        # Run alemtuzumabProcessing function to split the population into 
        # relevant subgroups
           
          alemtuzumabPopulation <- alemtuzumabProcessing(population = alemtuzumabPopulation,
                                                        nextCycleDMTColname = c(nextCycleDMTColumn),
                                                        cycleNum = cycleNum)
          
          
          alemtuzumabSwitch_intolerance <- 
            alemtuzumabPopulation$alemtuzumabSwitch_intolerance
                           
          alemtuzumabSwitch_clinicalActivity <- 
            alemtuzumabPopulation$alemtuzumabSwitch_clinicalActivity
          
          alemtuzumabSwitch_ADAs <-  
            alemtuzumabPopulation$alemtuzumabSwitch_ADAs
             
          alemtuzumab_continuing <-
            alemtuzumabPopulation$alemtuzumab_continuing
          
          remove(alemtuzumabPopulation)
        

        } else { # If nobody on alemtuzumab create blank DTs for rbind
          
          alemtuzumabSwitch_intolerance <- emptyDT
          alemtuzumabSwitch_ADAs <- emptyDT
          alemtuzumab_continuing <- emptyDT
          alemtuzumabSwitch_clinicalActivity <- emptyDT
          
        }
        

     # -------------------------------------------------------------------------
    
      
                
    # --------------------------------------------------------------------------
    # Process natalizumab - risk of PML important
    # ----

      if(nrow(natalizumabPopulation) > 0){

        natalizumabPopulation <- natalizumabProcessing(population = natalizumabPopulation,
                                                       currentCycleEDSSCol = currentCycleEDSSColumn, 
                                                       nextCycleDMTColname = c(nextCycleDMTColumn),
                                                       cycleNum = cycleNum)

        natalizumab_PMLDeaths <- natalizumabPopulation$natalizumab_PMLDeaths
        natalizumabSwitch_clinicalActivity <- natalizumabPopulation$natalizumabSwitch_clinicalActivity
        natalizumabSwitch_intolerance <- natalizumabPopulation$natalizumabSwitch_intolerance
        natalizumab_continuing <- natalizumabPopulation$natalizumab_continuing

      } else { # If there are none on natalizumab create blank DT for rbind later

        natalizumab_PMLDeaths <- emptyDT
        natalizumabSwitch_clinicalActivity <- emptyDT
        natalizumabSwitch_intolerance <- emptyDT
        natalizumab_continuing <- emptyDT

      }


    # --------------------------------------------------------------------------

    
        
    # --------------------------------------------------------------------------
    # Process all other DMTs that don't need specific processes
    # ----      
    
       if(nrow(otherDMTPopulation) > 0){
    
        otherDMTPopulation <- otherDMTsProcessing(population = otherDMTPopulation,
                                                  nextCycleDMTColname = c(nextCycleDMTColumn),
                                                  cycleNum = cycleNum)

        otherDMTsSwitch_clinicalActivity <- otherDMTPopulation$otherDMTsSwitch_clinicalActivity
        otherDMTsSwitch_intolerance <- otherDMTPopulation$otherDMTsSwitch_intolerance
        otherDMTs_continuing <- otherDMTPopulation$otherDMTs_continuing
          
      } else { # If there are none on natalizumab create blank DT for rbind later
     
        otherDMTsSwitch_clinicalActivity <- emptyDT
        otherDMTsSwitch_intolerance <- emptyDT
        otherDMTs_continuing <- emptyDT
        
      }
        
        
    # --------------------------------------------------------------------------
        
        
        
    # --------------------------------------------------------------------------
    # Combine populations for common processes
    # --------------
        
        clinicalActivitySwitches <- rbind(cladribineSwitch_clinicalActivity,
                                          alemtuzumabSwitch_clinicalActivity,
                                          natalizumabSwitch_clinicalActivity,
                                          otherDMTsSwitch_clinicalActivity
                                          )
        
        intoleranceSwitches <- rbind(alemtuzumabSwitch_ADAs,
                                     cladribineSwitch_intolerance,
                                     alemtuzumabSwitch_intolerance,
                                     natalizumabSwitch_intolerance,
                                     otherDMTsSwitch_intolerance)
        
        continuingPopulation <- rbind(cladribine_continuing,
                                      alemtuzumab_continuing,
                                      natalizumab_continuing,
                                      otherDMTs_continuing)
        
        
        
      # Define lengths for checking
      
        clinicalActivitySwitches_length <- nrow(clinicalActivitySwitches)
        
        intoleranceSwitches_length <- nrow(intoleranceSwitches)
        
        continuingPopulation_length <- nrow(continuingPopulation)
        
        natalizumab_PMLDeaths_length <- nrow(natalizumab_PMLDeaths)
        
        previouslyWithdrawnPopulation_length <- nrow(previouslyWithdrawnPopulation)
        
        EDSSOver7Population_length <- nrow(EDSSOver7Population)

                                           

      # Check

        checkNum <- Reduce(`+`, mget(ls(pattern = ".*_length$")))
        
        if(abs(RRMSPopulationSize - checkNum) >= 1) {
        
            stop(' !!! Discrepancy in population sizes after DMT processing. !!! ')
        
          }
        
                                           
                
        
    # --------------------------------------------------------------------------
    
     
        
    # --------------------------------------------------------------------------
    # Split into RES, less severe ongoing activity, and also ongoing activity
    # in those who were previously experiencing RES                                                
    # -------      
          
      # Rapidly Evolving Severe (>=2 relapses in a year)
      
        RESPopulation <- 
          clinicalActivitySwitches[clinicalActivity >= 2]
        
        RESPopulation[, RRMSType := 'RES']
        
        ongoingActivityPopulation <- clinicalActivitySwitches[clinicalActivity == 1]
        
        
      # Ongoing activity - split into RES or previous RES, or not
      
        ongoingActivityPopulation_RRMS <- 
          ongoingActivityPopulation[existingRES == 0L]
        
        ongoingActivityPopulation_RES <- 
          rbind(ongoingActivityPopulation[existingRES == 1L],
                RESPopulation)
        
        
    # --------------------------------------------------------------------------
        


    # --------------------------------------------------------------------------
    # Update switching tables to remove an individual's current DMT from
    # all tables so that they cannot be given that DMT again
    # ----
        
      
      # Create combined DT of switching population
      
          switchers <- rbind(ongoingActivityPopulation_RRMS[, .(interventionID, parameterSetID, currentCycleDMT)],
                             ongoingActivityPopulation_RES[, .(interventionID, parameterSetID,currentCycleDMT)],
                             intoleranceSwitches[, .(interventionID, parameterSetID, currentCycleDMT)])
        
      if (nrow(switchers) > 0){
        
      # Merge in DMTs that are similar to current DMT in being the same but at 
      # a different dose or being in the same class
      
        includeLinkedDMTs <- linkedDMTs[switchers, 
                                        mult = 'all',
                                        on = .(dmtID = currentCycleDMT),
                                        allow.cartesian = TRUE]
      
      #-------------------------------------------------------------------------
      # This functionality is currently deprecated - it is the mechanism      --
      # to include bidirectional drug linkage but is not necessary as         --
      # the approach stands                   
        
          # includeLinkedDMTsForward <- linkedDMTs[switchers, 
          #                       mult = 'all', 
          #                       on = .(dmtID = currentCycleDMT), 
          #                       allow.cartesian = TRUE]
          # 
          # 
          # includeLinkedDMTsBackward <- linkedDMTs[switchers, 
          #                       mult = 'all', 
          #                       on = .(linkedDMTID = currentCycleDMT), 
          #                       allow.cartesian = TRUE]
          # 
          # includeLinkedDMTs <- rbind(includeLinkedDMTsForward, 
          #                            includeLinkedDMTsBackward)
                                      
      #-------------------------------------------------------------------------  
          
      # Melt and keep only unique interventionID/parameterSetID/dmtID combinations 
          includeLinkedDMTs <- 
            melt(includeLinkedDMTs[, .(interventionID, parameterSetID, dmtID, linkedDMTID)], 
                 id.vars = c('interventionID', 'parameterSetID'))[, .(interventionID, parameterSetID, dmtID = value)]
          
          
          includeLinkedDMTs <- unique(includeLinkedDMTs[!is.na(dmtID)])
          
      
      # Go through switching tables and remove current DMT and any similar
      # DMTs for each individual being switched
       
          switchingTableList <- lapply(switchingTableList, function(table) {
            DT <- table[!includeLinkedDMTs, on = c('interventionID', 'parameterSetID', 'dmtID')]
          })
          
      
      # Tidy 
          
          remove(includeLinkedDMTs)
    
      }
        
        remove(switchers)
        
    # --------------------------------------------------------------------------
 
      
      
    # --------------------------------------------------------------------------
    # Switch DMTs for intolerance                                          
    # ---- 
    
   
    # Split intolerant population into RRMS, RES and first/second line ---------
          
          
      # No previous RES, on first line
      
          intolerantPopulation_RRMSFirstLine <- 
            intoleranceSwitches[existingRES == 0L & treatmentLine == 1L]
          
      
      # No previous RES, on second line treatment 
          
          intolerantPopulation_RRMSSecondLine <- 
            intoleranceSwitches[existingRES == 0L & treatmentLine == 2L]
          
          
      # No previous RES, on third line treatment 
          
          intolerantPopulation_RRMSThirdLine <- 
            intoleranceSwitches[existingRES == 0L & treatmentLine > 2L]
          
          
          
      # Previous RES, on first line
        
          intolerantPopulation_RESFirstLine <- 
            intoleranceSwitches[existingRES == 1L & treatmentLine == 1L]
          
          
      # Previous RES, on second line treatment
        
          intolerantPopulation_RESSecondLine <- 
            intoleranceSwitches[existingRES == 1L & treatmentLine == 2L]
          
          
       # Previous RES, on third line treatment
        
          intolerantPopulation_RESThirdLine <- 
            intoleranceSwitches[existingRES == 1L & treatmentLine > 2L]
          
      
      # Create empty data table to hold switching outcomes
          
          intolerantPopulation_Switched <- emptyDT
          
      
      # Remove unsplit intolerant population
          
          remove(intoleranceSwitches)
          
      
    # ----
 
       # Switch RRMS First line
        
        if (nrow(intolerantPopulation_RRMSFirstLine) > 0 ) {

          intolerantPopulation_RRMSFirstLine <- 
            
            switchFunction(intolerantPopulation_RRMSFirstLine, 
                           'RRMSFirstLineIntolerance',
                           switchingTableList = switchingTableList,
                           cycle = cycleNum)

          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RRMSFirstLine)
                
          remove(intolerantPopulation_RRMSFirstLine)
          
        }
       
          
          
      # Switch RRMS Population on second line

        if (nrow(intolerantPopulation_RRMSSecondLine) >0 ) {
        
          
          intolerantPopulation_RRMSSecondLine <- 
          
            switchFunction(intolerantPopulation_RRMSSecondLine, 
                         'RRMSFirstLineClinicalFailure',
                         switchingTableList = switchingTableList,
                          cycle = cycleNum)
        
          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RRMSSecondLine)

          
          remove(intolerantPopulation_RRMSSecondLine)
          
        }   


      # Switch RRMS Population on third line

        if (nrow(intolerantPopulation_RRMSThirdLine) >0 ) {
        
          
          intolerantPopulation_RRMSThirdLine <- 
          
            switchFunction(intolerantPopulation_RRMSThirdLine, 
                         'RRMSSecondLineClinicalFailure',
                         switchingTableList = switchingTableList,
                          cycle = cycleNum)
        
          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RRMSThirdLine)

          
          remove(intolerantPopulation_RRMSThirdLine)
          
        }             
          
          
      # Switch RES first line
          
        if (nrow(intolerantPopulation_RESFirstLine) > 0 ) {

          intolerantPopulation_RESFirstLine <- 
            
            switchFunction(intolerantPopulation_RESFirstLine, 
                           'RESFirstLineIntolerance',
                           switchingTableList = switchingTableList,
                           cycle = cycleNum)

          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RESFirstLine)
                
          remove(intolerantPopulation_RESFirstLine)
          
        }
       
          
          
      # Switch RES Population on second line

        if (nrow(intolerantPopulation_RESSecondLine) >0 ) {
        
          
          intolerantPopulation_RESSecondLine <- 
          
            switchFunction(intolerantPopulation_RESSecondLine, 
                         'RESFirstLineClinicalFailure',
                         switchingTableList = switchingTableList,
                          cycle = cycleNum)
        
          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RESSecondLine)

          
          remove(intolerantPopulation_RESSecondLine)
          
        }   


      # Switch RES Population on third line

        if (nrow(intolerantPopulation_RESThirdLine) >0 ) {
        
          
          intolerantPopulation_RESThirdLine <- 
          
            switchFunction(intolerantPopulation_RESThirdLine, 
                         'RESSecondLineClinicalFailure',
                         switchingTableList = switchingTableList,
                          cycle = cycleNum)
        
          
          intolerantPopulation_Switched <- 
            rbind(intolerantPopulation_Switched,
                  intolerantPopulation_RESThirdLine)

          
          remove(intolerantPopulation_RESThirdLine)
          
        } 
          
      
        
        # Assign switched drug to nextCycleDMTColumn
          
        if (nrow(intolerantPopulation_Switched) > 0 ){
          
          intolerantPopulation_Switched[, c(nextCycleDMTColumn) := switch]
          intolerantPopulation_Switched[, DMTByCycle := paste0(DMTByCycle, "i.")]
          
          # If switch is to withdrawn mark intolerance as reason for withdrawal
          intolerantPopulation_Switched[switch == withdrawnID, DMTWithdrawalReason := 'Intolerance']

        }
          
          
        # Check population size
        
          if(abs(nrow(intolerantPopulation_Switched) - intoleranceSwitches_length) > 0){
            stop(' !!! Difference in size of incoming/outgoing intolerance switching population !!! ')
          }
          
    # --------------------------------------------------------------------------
  

              
    # --------------------------------------------------------------------------
    # Switch DMTs for clinical activity
    # ----
 
       clinicalActivityPopulation_Switched <- emptyDT   
          
          
    # Split ongoing activity population into RRMS, RES and first/second line ---
      
      # RRMS    
      ongoingActivityPopulation_RRMSFirstLineFailure <- 
          ongoingActivityPopulation_RRMS[treatmentLine == 1L]
      
      
      ongoingActivityPopulation_RRMSSecondLineFailure <- 
          ongoingActivityPopulation_RRMS[treatmentLine == 2L]
      
      
      ongoingActivityPopulation_RRMSThirdLineFailure <- 
          ongoingActivityPopulation_RRMS[treatmentLine == 3L]
      
      
      # RES
      ongoingActivityPopulation_RESFirstLineFailure <- 
        ongoingActivityPopulation_RES[treatmentLine == 1L]
      
      
      ongoingActivityPopulation_RESSecondLineFailure <- 
        ongoingActivityPopulation_RES[treatmentLine == 2L]
      
      
      ongoingActivityPopulation_RESThirdLineFailure <- 
        ongoingActivityPopulation_RES[treatmentLine == 3L]
      
      
      # First line failure with ongoing non-RES RRMS activity
          
        if(nrow(ongoingActivityPopulation_RRMSFirstLineFailure) > 0 ){
          
          
          ongoingActivityPopulation_RRMSFirstLineFailure <-
            
           switchFunction(ongoingActivityPopulation_RRMSFirstLineFailure, 
                         'RRMSFirstLineClinicalFailure',
                          switchingTableList = switchingTableList,
                          cycle = cycleNum)

            
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RRMSFirstLineFailure)
           
            remove(ongoingActivityPopulation_RRMSFirstLineFailure)
                  

        }
      
      
      # Second line failure with ongoing non-RES RRMS activity
          
        if(nrow(ongoingActivityPopulation_RRMSSecondLineFailure) > 0 ){
          
          
          ongoingActivityPopulation_RRMSSecondLineFailure <-
            
           switchFunction(ongoingActivityPopulation_RRMSSecondLineFailure, 
                         'RRMSSecondLineClinicalFailure',
                          switchingTableList = switchingTableList,
                          cycle = cycleNum)

            
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RRMSSecondLineFailure)
           
            remove(ongoingActivityPopulation_RRMSSecondLineFailure)
                  

        }
         
      
          
      # First line failure with ongoing RES level of activity
          
        if(nrow(ongoingActivityPopulation_RESFirstLineFailure) > 0 ){
          
          
          ongoingActivityPopulation_RESFirstLineFailure <-
            
           switchFunction(ongoingActivityPopulation_RESFirstLineFailure, 
                         'RESFirstLineClinicalFailure',
                          switchingTableList = switchingTableList,
                          cycle = cycleNum)

            
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RESFirstLineFailure)
           
            remove(ongoingActivityPopulation_RESFirstLineFailure)
                  

        }
      
      
      # Second line failure with ongoing RES level of activity
          
        if(nrow(ongoingActivityPopulation_RESSecondLineFailure) > 0 ){
          
          
          ongoingActivityPopulation_RESSecondLineFailure <-
            
           switchFunction(ongoingActivityPopulation_RESSecondLineFailure, 
                         'RESSecondLineClinicalFailure',
                          switchingTableList = switchingTableList,
                          cycle = cycleNum)

            
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RESSecondLineFailure)
           
            remove(ongoingActivityPopulation_RESSecondLineFailure)
                  

        }
      
      
      # Withdraw for treatment failure beyond treatment line 3
          
        if(nrow(ongoingActivityPopulation_RRMSThirdLineFailure) > 0 ){
        
        
            ongoingActivityPopulation_RRMSThirdLineFailure[, `:=`
                                                           (switch = withdrawnID,
                                                             DMTsWithdrawn = 1L,
                                                             DMTWithdrawalReason = 'Treatment Failure')] 
      
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RRMSThirdLineFailure)
          
        }
      
      
        if(nrow(ongoingActivityPopulation_RESThirdLineFailure) > 0 ){
        
        
            ongoingActivityPopulation_RESThirdLineFailure[, `:=`
                                                           (switch = withdrawnID,
                                                             DMTsWithdrawn = 1L,
                                                             DMTWithdrawalReason = 'Treatment Failure')] 
      
           clinicalActivityPopulation_Switched <- rbind(
             clinicalActivityPopulation_Switched,
             ongoingActivityPopulation_RESThirdLineFailure)
        }
      
    
      # Assign switched drug to nextCycleDMTColumn and 
      # increment treatment line
          
      if (nrow(clinicalActivityPopulation_Switched) > 0 ){
        
          clinicalActivityPopulation_Switched[, c(nextCycleDMTColumn) 
                                              := switch]
          clinicalActivityPopulation_Switched[, treatmentLine 
                                              := treatmentLine + 1L]
          
          clinicalActivityPopulation_Switched[, DMTByCycle := paste0(DMTByCycle, "r.")]

          
      }
        
      
       # Check population size
        
        if(abs(nrow(clinicalActivityPopulation_Switched) - clinicalActivitySwitches_length) > 0){
          browser()
          stop(' !!! Difference in size of incoming/outgoing clinical activity switching population !!! ')
        }    
          
  # ----------------------------------------------------------------------------        

  
   
  # --------------------------------------------------------------------------
  # Record changes 
  # ----  
          
    # Create single switched population and remove components
      
        switchedPopulation <- rbind(clinicalActivityPopulation_Switched,
                                      intolerantPopulation_Switched)
          
        remove(clinicalActivityPopulation_Switched,
                intolerantPopulation_Switched)
    
          
    # Note new drug in sequence, and reset time on DMT clock (mark specially if 
    # alemtuzumab)
      
        switchedPopulation[, `:=` (timeOnCurrentDMT = 1L,
                                   currentDMTDoses = 1L)]
        switchedPopulation[get(nextCycleDMTColumn) == alemtuzumabID |
                           get(nextCycleDMTColumn) == alemtuzumabCompleteID |
                           get(nextCycleDMTColumn) == alemtuzumabCourse1CompleteID |  
                           get(nextCycleDMTColumn) == alemtuzumabDose2ID,
                              alemtuzumabDoses := 
                                     ifelse(is.na(alemtuzumabDoses), 
                                            1,
                                            alemtuzumabDoses)]
                                   
        switchedPopulation[, DMTSequence := paste0(DMTSequence,
                                                   switch, "in", cycleNum, " ")]
        
        switchedPopulation[, activityAtSwitch :=
                             paste0(activityAtSwitch, ", ", clinicalActivity, " in ", 
                                    cycleNum, 'Existing RES', existingRES)]
        
        
    # Increment time on DMT for those not switched and copy
    # across last cycle's DMT to this cycle. NB for alemtuzumab this record
    # is made in the switching process
        
        continuingPopulation[, timeOnCurrentDMT := 
                                            timeOnCurrentDMT + 1L]
      
        
    # Assign withdrawn population withdrawn again for next cycle
      
        previouslyWithdrawnPopulation[, c(nextCycleDMTColumn) := 
                                           withdrawnID]

        
    # Recreate RRMS Population and tidy 
           
        RRMSPopulation <- rbind(previouslyWithdrawnPopulation,
                                EDSSOver7Population,
                                continuingPopulation,
                                natalizumab_PMLDeaths,
                                switchedPopulation)
        
        RRMSPopulation[get(nextCycleDMTColumn) != alemtuzumabID &
                       get(nextCycleDMTColumn) != alemtuzumabDose2ID &
                       get(nextCycleDMTColumn) != alemtuzumabCourse1CompleteID &   
                       get(nextCycleDMTColumn) != alemtuzumabCompleteID, 
                       DMTByCycle := paste0(DMTByCycle,
                                            cycleNum, ': ',
                                            get(nextCycleDMTColumn), ", ")]
        
        RRMSPopulation[get(nextCycleDMTColumn) == alemtuzumabID |
                       get(nextCycleDMTColumn) == alemtuzumabDose2ID |
                       get(nextCycleDMTColumn) == alemtuzumabCourse1CompleteID |  
                       get(nextCycleDMTColumn) == alemtuzumabCompleteID,
                       DMTByCycle := paste0(DMTByCycle,
                                            cycleNum, ': ',
                                            get(nextCycleDMTColumn),
                                            " (", alemtuzumabDoses, "), ")]


        RRMSPopulation[, `:=` (DMTIntolerance = NULL,
                               existingRES = NULL,
                               switch = NULL,
                               clinicalActivity = NULL)]
        
        
    #  Mark time on alemtuzumab
    
        RRMSPopulation[get(nextCycleDMTColumn) == alemtuzumabID |
                       get(nextCycleDMTColumn) == alemtuzumabDose2ID |
                       get(nextCycleDMTColumn) == alemtuzumabDose3ID |
                       get(nextCycleDMTColumn) == alemtuzumabCourse1CompleteID | 
                       get(nextCycleDMTColumn) == alemtuzumabCompleteID,
                       timeOnAlemtuzumab := timeOnAlemtuzumab + 1]
        
        
    #  Mark time on any DMT for those 
    
        RRMSPopulation[!is.na(get(nextCycleDMTColumn)) & 
                         get(nextCycleDMTColumn) != withdrawnID, 
                       timeOnAnyDMT := timeOnAnyDMT + 1]


        

  # ----------------------------------------------------------------------------        
         
  
         
  # Check no patients lost
        
    if (RRMSPopulationSize != nrow(RRMSPopulation)) {
      
     print(paste0(RRMSPopulationSize, '\n'))
           print(paste0(nrow(RRMSPopulation), '\n'))

      stop('Mismatch in incoming/outgoing population size')
      
    }

  # Return

    return(list(RRMSPopulation = RRMSPopulation, switchingTables = switchingTableList))
}
