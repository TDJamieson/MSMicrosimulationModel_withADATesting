#==============================================================================#
#                                                                              #
#---                   Assign RRMS to SPMS Transitions                      ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Transitions from RRMS to SPMS are based on current EDSS state using a        #
# probability matrix with only one EDSS state that can be transitioned 'To'    #
# i.e., each 'From' row vector has zeroes in all but one cell corresponding to #
# a 'To' EDSS. The non-zero probability of transition is always to an EDSS     #
# state one greater than the 'From' EDSS.                                      #
#                                                                              #
# It is assumed, in line with the bulk of literature, that DMTs have no        # 
# effect in SPMs, and therefore treatment effects do not need to be applied.   #
#                                                                              #
#==============================================================================#

# The process of determining transition is:
# - Merge in for each RRMS patient the row vector for their 'From' EDSS
# - Cumulatively sum row-wise the probabilities
# - Create and a runif (0,1) for each individual
# - Melt these so that each individual has a long table with an entry for each 
#   'To' EDSS with a cumsum value, and next to that their runif replicated
# - The random number is compared with the cumsum value, and if the random
#   number is less than the cumsum probability it is flagged 
# - The first ocurrence of that is taken as the 'To' EDSS, implying that if the 
#   random number is greater than the transition probability, nothing will be 
#   returned and the individual will end up with an NA
#   

assignRRMSSPMSTransitions <- function(RRMSPopulation = RRMSSubset,
                                      currentCycleEDSSCol = currentCycleSPMSEDSSCol,
                                      cycle = cycle,
                                      nextCycleDMTColumn = nextCycleDMTCol) {
   
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      print('Generating transitions from RRMS to SPMS')
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 
   
 # Merge in EDSS-specific RRMS to SPMS transition vector
 
      RRMStoSPMSDT <-
        RRMSPopulation[, .(interventionID, lastCycleEDSS, parameterSetID,
                           RRMStoSPMSTransitionOutcomeRandNo)][transitionMatrix_RRMStoSPMSDT,
                         on = .(lastCycleEDSS = From, parameterSetID),
                         `:=` ('0' = i.0,
                               '1' = i.1,
                               '2' = i.2,
                               '3' = i.3,
                               '4' = i.4,
                               '5' = i.5,
                               '6' = i.6,
                               '7' = i.7,
                               '8' = i.8,
                               '9' = i.9)]
      
      

 # Reduce to produce cumsum (separate process required for a single record    --
 # because data.table returns a vector rather than a list)                    --
 
    if (nrow(RRMStoSPMSDT) == 1) {
        
      RRMStoSPMSDT[, c(as.character(0:9)) := as.list(
                       Reduce(`+`, .SD, accumulate = TRUE)),
                      .SDcols = c(as.character(0:9))]

    } else {

      RRMStoSPMSDT[, c(as.character(0:9)) :=
                     Reduce(`+`, .SD, accumulate = TRUE),
                    .SDcols = c(as.character(0:9))]
    }
      

  # Create melted DT                                                          --

     RRMStoSPMSDT <-
      melt(RRMStoSPMSDT[, c('interventionID', 'parameterSetID',
                            'RRMStoSPMSTransitionOutcomeRandNo',
                             as.character(c(0:9)))],
         id.vars = c('interventionID', 'parameterSetID', 'RRMStoSPMSTransitionOutcomeRandNo'))

     
  # Where the value is 0 set it to -1 so that these do not contribute,        --
  # otherwise the first 'To' where RRMStoSPMSTransitionOutcomeRandNo < p(transition)    --
  # would be 0 each time
       
     RRMStoSPMSDT[value == 0, value := -1]
     setorder(RRMStoSPMSDT, parameterSetID, interventionID, variable)
     
  
  # Select first record where the random number is less than the              --
  # the cumulative probability as this is the appropriate 'To' EDSS           --

     RRMStoSPMSDT <-
       RRMStoSPMSDT[(RRMStoSPMSTransitionOutcomeRandNo - value) < 1e-10,
                           .SD[1],
                           by = .(interventionID, parameterSetID)]

     
  # Redefine the variable value as an integer; this is their 'To' EDSS        --
   
       RRMStoSPMSDT[, variable := as.integer(as.character(variable))]
       
       
  # There is now a data table with a single entry for each individual where   --
  # their random number was <= the probability of transition to SPMS, and     --
  # this single entry contains their ID and the value of the EDSS to which    --
  # they are transitioning (in SPMS). Those with a random number greater than --
  # the transition probability have no entry.  This data table is merged with --
  # the RRMSSubset DT on ID, with the 'variable' value containing their 'To'  --
  # EDSS being assigned to the current cycle EDSS column.  Those that do not  --
  # have an entry in the RRMStoSPMSDT, those not transitioning will have an  --
  # NA filled in this field.
       
     RRMSPopulation[RRMStoSPMSDT,
              c(currentCycleEDSSCol) := i.variable,
                    on = .(interventionID, parameterSetID)]
     
     
  # If their outcome EDSS is the same as their 'From' EDSS then they should   --
  # be dropped since they are not transitioning (needs an expression created
  # to evaluate dynamic column)
  
       expr <- parse(text = paste0(currentCycleEDSSCol, "== lastCycleEDSS"))
  
       RRMSPopulation[eval(expr), 
                      c(currentCycleEDSSCol) := NA]

  
  # Identify and mark those that have transitioned                            --
  
      RRMSPopulation[!is.na(get(currentCycleEDSSCol)), `:=`
                          (SPMSCohort = 1L,
                           timeToSPMS = cycle,
                           RRMSCohort = 0L)]
      
  
  # Withdraw DMTs from those that have transitioned to SPMS                   --
    
      RRMSPopulation[SPMSCohort == 1L, 
                     c(nextCycleDMTColumn) := withdrawnID]
      
      RRMSPopulation[SPMSCohort == 1L, DMTWithdrawalReason := 'SPMS Transition']

  
  # Tidy

      remove(RRMStoSPMSDT)
      
    
  # Return
      
      return(RRMSPopulation)

}
      
# ==============================================================================

     
