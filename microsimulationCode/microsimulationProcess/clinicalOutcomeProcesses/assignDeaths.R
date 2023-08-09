
#==============================================================================#
#                                                                              #
#---                           Assign Deaths                                ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Mortality risk is based on age and gender, taken from UK ONS life tables.    #  
# This baseline stratified risk is modified by a mortality multiplier specific #
# to the EDSS state of an individual, with increasing EDSS giving increasing   #
# mortality ratios. A mortality risk table for each age, gender, and EDSS      #
# is created in 'deriveInputs.R' based on inputs from the 'inputMatrices'      #
# spreadsheet.                                                                 #
#                                                                              #
#==============================================================================#
 

assignDeaths <- function (population = workingSample, cycleNum = cycle) {
    
 
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    print('Assigning Deaths')
# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


  # Merge age-, gender- and EDSS-specific mortality rate                      --

      population[mortalityRateTable,
                    stratifiedMortalityRate := i.mortalityRate,
                    on = .(currentAge = age, 
                           femaleGender = femGender)]
       
 
  # Merge EDSS-specific multiplier                                            --
       
      population[mortalityEDSSMultiplierDT[, .(EDSS, Multiplier, parameterSetID)],
                 multiplier := i.Multiplier,
                 on = .(lastCycleEDSS = EDSS, parameterSetID)]

  
      
  # Apply mortality multiplier and convert modified rate to risk/probability  --
  
      population[, modifiedMortalityRisk := 
                   rateToProbability(stratifiedMortalityRate * multiplier)]
      
      
     
  # In those who have not already been excluded from processing through       --                          
  # reaching age 100 or having died (who will have a traceComplete dummy      --
  # value of 1), assign death by testing for                                  --
  # mortalityOutcome_randNo <= modified mortality risk (probability)          --
                  
     population[is.na(traceComplete) &
                      (mortalityOutcomeRandNo - modifiedMortalityRisk < 1e-10), 
                    Death := 1L]
   
    
     
  # If an individual has now died fill their time to death with the current   --
  # cycleNum number - 1, their ageAtDeath with their currentAge, and ensure   --
  # that they are removed from the RRMS and SPMS cohorts.                     --
  
      population[Death == 1L & is.na(traceComplete), `:=` 
                    (timeToDeath = cycleNum-1,
                     ageAtDeath = currentAge,
                     RRMSCohort = 0L,
                     SPMSCohort = 0L)]
      
    
  # If an individual has just died and is currently on a DMT, mark death as   --
  # their DMT withdrawal reason                                               --  
      
      population[Death == 1L & is.na(traceComplete) & currentCycleDMT != withdrawnID,
                 DMTWithdrawalReason := 'Death']
      
      
  # Mark those that have just died as traceCopmplete so they do not continue  --
  # being simulated                                                           --
      
      population[Death == 1L & is.na(traceComplete), traceComplete := 1L] 
                 
  
                       
  # Tidy
    
      population[, c('modifiedMortalityRisk',
                     'stratifiedMortalityRate',
                     'mortalityOutcomeRandNo',
                     'multiplier') := NULL]
 

      
# Progress output -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  print(paste0(nrow(population[
    timeToDeath == cycleNum-1]),
        ' Deaths this cycle')
        )
# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
  
# Outcome checks *************************************************************
    
    mortalityOutcomesDT <- 
      population[, .(noDeaths = sum(Death, na.rm = TRUE),
                   noIndividuals = as.integer(.N),
                   Cycle = cycleNum),
                  by = .(currentAge, femaleGender, seedGroup,
                         lastCycleEDSS, parameterSetID, interventionCohort)]
    
    mortalityOutcomesDT[, propDeaths := noDeaths/noIndividuals,
             by = .(lastCycleEDSS, currentAge, femaleGender, seedGroup,
                    parameterSetID, interventionCohort)]
    
    
 # *****************************************************************************
  
    
  # Return
  
      return(list(population = population, 
                  mortalityOutcomesDT = mortalityOutcomesDT))
   
}


#===============================================================================
