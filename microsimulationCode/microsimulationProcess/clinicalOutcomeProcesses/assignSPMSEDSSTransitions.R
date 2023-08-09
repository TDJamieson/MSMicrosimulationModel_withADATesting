#==============================================================================#
#                                                                              #
#---              Determine EDSS transitions for those in SPMS              ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# EDSS transitions are based on matrix with a To and From EDSS value with a    #
# zero or non-zero probability for each To/From pairs. In SPMS there are no    #
# treatment effects to be applied so the annual probabilities in the           #
# transition probability matrix in 'inputMatrices.xlslx' can be used without   #
# processing.                                                                  #
#                                                                              #      
# The process of assigning transitions is the same as for RRMS EDSS            #
# without the requirement for the steps that modify the transition             #
# probabilities in the RRMS EDSS process.  That process is:                    #
#                                                                              #
# - Merge in a transition probability vector on EDSS i.e. lastCycleEDSS        #
#   of 8 gets a vector with probabilities taken from the 8 row in the          #
#   transition probability matrix to give a set of probabilities for each      #
#   'To' EDSS related to a 'From' of 8                                         #
#                                                                              #
# - The probabilities are cumulatively summed from 0-9                         #
#                                                                              #
# - A runif (0,1) is generated to determine outcome                            #
#                                                                              #
# - The DT is melted so that each individual has a set of rows with the cumsum #
#   probabilities, alongside the random number.                                #
#                                                                              #
# - This is ordered by EDSS and then the first row where the random number is  #
#   <= to the probability but > the previous rows(EDSS) probability is taken   #
#                                                                              #
#==============================================================================#

assignSPMSEDSSTransitions<- function(SPMSPopulation = SPMSSubset,
                                     currentCycleEDSSCol = currentCycleSPMSEDSSCol) {
  

# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
                print(paste0('Generating SPMS EDSS transitions in ',
                             nrow(SPMSPopulation),
                             ' in SPMS group')
                )
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*
   
    # Merge in transition probabilities by 'From' EDSS - dependent on PSA     --
    # level
  
    if(PSA_Switch == 1 & individualLevelPSA_probabilisticSwitch == 1){

        SPMSEDSSTransitionDT <-
          SPMSPopulation[, .(interventionID, parameterSetID, 
                             lastCycleEDSS, SPMSEDSSTransitionOutcomeRandNo)][
            transitionMatrix_EDSS_SPMSDT,
            on = .(lastCycleEDSS = From, interventionID, parameterSetID),
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
        
        
    } else {
      
          SPMSEDSSTransitionDT <-
          SPMSPopulation[, .(interventionID, parameterSetID, 
                             lastCycleEDSS, SPMSEDSSTransitionOutcomeRandNo)][
            transitionMatrix_EDSS_SPMSDT,
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
          
    }
      
      



    # Create cumsum along probabilities NB data.table doesn't return a list   --   
    # where there is only one row so needs to be explicitly as.list
         
        if (nrow(SPMSEDSSTransitionDT) == 1) {
              SPMSEDSSTransitionDT[,  c(as.character(0:9)) := as.list(
                                       Reduce(`+`, .SD, accumulate = TRUE)),
                                      .SDcols = c(as.character(0:9))]

               } else {

              SPMSEDSSTransitionDT[, c(as.character(0:9)) :=
                             Reduce(`+`, .SD, accumulate = TRUE),
                            .SDcols = c(as.character(0:9))]
               }


    # Melt long so that each individual has a column vector of each           --
    # EDSS 'To' and the cumulative probability for that 'To'.                 --
    # Then take the shifted 'To' from the next EDSS up so that each           --
    # EDSS value has the starting and ending values for the                   --
    # cumulative probability,. e.g. EDSS 0: 0.145 - 0.25                      --
    #                               EDSS 1: 0.25 - 0.344...                   --
    #                            ...EDSS 9: 0.986 - 1.00                      --
    # These ranges can then be compared with the runiform created             --
    # above to determine where their new EDSS should be                       --

    # Melt

        SPMSEDSSTransitionDT <-
          melt(SPMSEDSSTransitionDT[, c('interventionID', 'parameterSetID', 
                                        'SPMSEDSSTransitionOutcomeRandNo',
                                        as.character(c(0:9)))],
                id.vars = c('interventionID', 'parameterSetID', 
                            'SPMSEDSSTransitionOutcomeRandNo'))


    # Order
    
        setorder(SPMSEDSSTransitionDT, interventionID, parameterSetID, variable)
    
        
    # Assign -1 to all zeroes so that they don't contribute to comparison     --
     
       SPMSEDSSTransitionDT[value == 0, value := -1]

       
    # Take first record satisfying condition and define EDSS as integer       --
    
       SPMSEDSSTransitionDT <-
          SPMSEDSSTransitionDT[(SPMSEDSSTransitionOutcomeRandNo - value) < 1e-10,
                               .SD[1],
                               by = .(interventionID, parameterSetID)]

        SPMSEDSSTransitionDT[, variable := as.integer(as.character(variable))]



    # Merge with working sample on id taking only records where   ----
    # there is a value for the outcome

        SPMSPopulation[SPMSEDSSTransitionDT,
                    c(currentCycleEDSSCol) := i.variable,
                    on = .(interventionID, parameterSetID)]
        
    
    # Tidy 
    
        remove(SPMSEDSSTransitionDT)
        
    
    # Return
    
        return(SPMSPopulation)


}

# ==============================================================================
