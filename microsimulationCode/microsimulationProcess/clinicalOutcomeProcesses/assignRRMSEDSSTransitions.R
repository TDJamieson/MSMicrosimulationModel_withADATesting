
#==============================================================================#
#                                                                              #
#---              Determine EDSS transitions for those in RRMS              ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# EDSS transitions are based on matrix with a To and From EDSS value with a    #
# non-zero probability for most To/From pairs. These probabilities need to be  #
# converted to rates in order for e.g. DMT effects to be applied. This is done #
# at the beginning, once, producing a transition rate matrix rather than       #
# transition probability matrix which is merged in here. After processing      #
# is complete the resulting processed rates need to be transformed back to     #
# probabilities.  This uses a function defined in 'defineFunctions.R           #
#                                                                              #
# In this process, DMT effects are applied, but applied only to EDSS levels    #
# above the current, 'From', EDSS i.e. if you're in EDSS 5 the DMT effect will #
# only reduce the probability of transition to EDSS levels 6 - 9. DMT effect   #
# is applied to the *rate* for these EDSS values, and these rates are then     #
# transformed back to *probabilities*.  With the new lower probabilities for   #
# higher 'To' EDSS levels, the probability of remaining in the current EDSS    #
# is recalculated by taking 1 - all other 'To' EDSS levels, which in effect    #
# is taking the reduced probability of progressing and assigning it to         #
# commensurately increase the probability of remaining in the current EDSS.    #
#                                                                              #
# The process of determining transition is the same as applied elsewhere in    #
# microsimulation, with the use of a runif (0,1) random no and a melted DT     #
# with cumsum of probabilities, though in this process because there is a      #
# probability for every To/From, the random number is assigned to the first    #
# 'TO' EDSS where the random number is less than the probability for that EDSS #
# but greater than the probability for the EDSS one below that.                #
#                                                                              #
#==============================================================================#

assignRRMSEDSSTransitions <- function(RRMSPopulation = RRMSSubset,
                                      RRMSEDSSTransitionMatrix_Rates = RRMSEDSSTransitionMatrix_Rates,
                                      currentCycleEDSSCol = currentCycleRRMSEDSSCol,
                                      cycleNum = cycle,
                                      lastCycleNum = lastCycle) {
 
         
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    print(paste0('Generating RRMS EDSS transitions in ',
                 nrow(RRMSPopulation),
                 ' remaining in RRMS')
    )
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 
  # ----------------------------------------------------------------------------       
  # Merge in individualised DMT effect for the DMT an individual was on at    --
  # the end of the last cycle. DMTRRMSEDSSProgressionEffectsDT holds values for -- 
  # this effect for each DMT for each individual.  This varies depending on   --
  # whether or not varying DMT effect by cycle is desired and has been set    --
  # in the controls spreadsheet.  If it has been set, then the table contains --
  # an effect for each individual for each cycle, either individualised or at --
  # parameter set level.  If varying DMT effects by cycle has not been set    --
  # then the table contains only one entry with one DMT effect for the whole  --
  # of the microsimulation.  In this case since merging is done for each      --
  # cycle then the name of the field containing the DMT effect must be        --
  # updated for each new cycle.                                               --
  # ------------------
 
  
 
        if(varyDMTEffectByCycle_Switch == 0){
  
          setnames(DMTRRMSEDSSProgressionEffectsDT, c(paste0('DMTRRMSEffect_Cycle', 
                                                           lastCycleNum)),
                   c(paste0('DMTRRMSEffect_Cycle', cycleNum)))
          
        }

        
        if(PSA_Switch == 1 & individualLevelPSA_probabilisticSwitch == 1){
          
          RRMSPopulation[DMTRRMSEDSSProgressionEffectsDT[, .(interventionID, dmtID, parameterSetID,
                      RRMSEDSSProgressionDMTEffect = get(paste0('DMTRRMSEffect_Cycle', cycleNum)))],
                      RRMSEDSSProgressionDMTEffect := i.RRMSEDSSProgressionDMTEffect,
                      on = .(interventionID, parameterSetID, currentCycleDMT = dmtID)]
          
          

        } else {
          
          RRMSPopulation[DMTRRMSEDSSProgressionEffectsDT[, .(dmtID, parameterSetID, interventionID,
                      RRMSEDSSProgressionDMTEffect = get(paste0('DMTRRMSEffect_Cycle', cycleNum)))],
                      RRMSEDSSProgressionDMTEffect := i.RRMSEDSSProgressionDMTEffect,
                      on = .(interventionID, parameterSetID, currentCycleDMT = dmtID)]
          
        }
        
  
    # Now remove these DMT effects from DMT DTs for memory efficiency 
    # (suppressed since single effect tables won't have these fields)
      
       if(varyDMTEffectByCycle_Switch == 1){
        
        DMTRRMSEDSSProgressionEffectsDT[, paste0('DMTRRMSEffect_Cycle', cycleNum) := NULL]
        
       }
        
  # ----------------------------------------------------------------------------
    
    
  # Merge in EDSS transition *rates* by lastCycleEDSS and under 28 onset      --
  # so that each individual has the vector of 'To' EDSS specific to their     --
  # 'From' EDSS and onset age in a set of columns in their row.               --
  # This depends on the level at which PSA is set - parameter set or          --
  # individual.
  
        
    if(PSA_Switch == 1 & individualLevelPSA_probabilisticSwitch == 1){

      RRMSEDSSTransitionDT <-
        RRMSPopulation[, .(interventionID, lastCycleEDSS, under28Onset, parameterSetID,
                      RRMSEDSSProgressionDMTEffect, RRMSEDSSTransitionOutcomeRandNo)][
          RRMSEDSSTransitionMatrix_Rates,
          on = .(lastCycleEDSS = From, interventionID, parameterSetID, 
                 under28Onset = under28),
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
      
      
      RRMSEDSSTransitionDT <-
        RRMSPopulation[, .(interventionID, lastCycleEDSS, under28Onset, parameterSetID,
                      RRMSEDSSProgressionDMTEffect, RRMSEDSSTransitionOutcomeRandNo)][
          RRMSEDSSTransitionMatrix_Rates,
          on = .(lastCycleEDSS = From, parameterSetID, 
                 under28Onset = under28),
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
        
  # With the RRMSEDSSTransitionMatrix rates merged in, reduce the probability --
  # for transition to higher EDSS states by applying the DMT effect.  This    --
  # requires multiplying the transition *rate* of all EDSS higher than the    --
  # 'From' EDSS, transforming these new rates back to probabilities, as well  --
  # as all the other unaltered 'To' rates, then recalculating the probability --
  # of remaining in the current state by taking 1 - prob(all other EDSS)      --


    # For loop to cycle through last cycle EDSS values
    
      for(currentEDSS in 0:8) {
  
      # Note which To EDSSs are above the current EDSS an individual is in    --
      # and which are the other EDSS states                                   --
  
        higherEDSSStates <- as.character(c((currentEDSS+1):9))
  
        otherEDSSStates <- c(as.character(0:9))
        otherEDSSStates <- otherEDSSStates[!otherEDSSStates %in% currentEDSS]


  
      # Multiply All 'To' columns (held in DT as character 0:9) above the     --
      # current EDSS by the DMT effect for the drug an individual is          --
      # currently on, the effect size of which was merged in at the           --
      # beginning of the cycle.  This is done by serially subsetting the      --
      # sample based on lastCycleEDSS values so that e.g. those with EDSS 7   --
      # in the last cycle, and who have 'higherEDSSStates' = c(8,9), are      --
      # selected separately from other individuals with different EDSS states --
      # in the last cycle and only have the DMT effect applied to those       --
      # columns, 8 & 9, which are held in 'higherEDSSStates'.                 --  

          RRMSEDSSTransitionDT[lastCycleEDSS == currentEDSS,
                                c(higherEDSSStates) :=
                                lapply(.SD, function(x) {

                                    x * RRMSEDSSProgressionDMTEffect

                                }),

                                .SDcols =  c(higherEDSSStates)]


      # Transform all rates to probabilities - excludes                       --
      # P(staying in current state) because this needs a new probability      --
      # calculated based on the other To/From transition probabilities.       --

          RRMSEDSSTransitionDT[lastCycleEDSS == currentEDSS,
                                c(otherEDSSStates) :=
                               lapply(.SD, rateToProbability),
                               .SDcols = c(otherEDSSStates)]



      # Using reduce, sum probability of transition to all EDSS states apart  --
      # from staying in current EDSS, and take 1- to give an updated          --
      # probability of remaining in the current state.                        --

          RRMSEDSSTransitionDT[lastCycleEDSS == currentEDSS,
                        c(as.character(currentEDSS)) := 1-Reduce(`+`, .SD),
                       .SDcols = c(otherEDSSStates)]


    }  # Close for loop


      # Convert rate to probability for group with the 'From' EDSS 9 since    --
      # this has no DMT effects to be applied so hasn't been processed above  --

          RRMSEDSSTransitionDT[lastCycleEDSS == 9,
                                   c(as.character(0:9)) :=
                                 lapply(.SD, rateToProbability),
                                 .SDcols = c(as.character(0:9))]



      # Now take each individual's row vector of transition probabilities,    --
      # which will be unique if probabilistic DMT effects have been used,     -- 
      # and use reduce with accumulation to produce a cumulative sum row      --
      # vector which can be used to compare against a runiform number to      --
      # assign the next EDSS. NB if there is only one record data table       --
      # handles lists differently, producing column vectors, so as.list       --
      # needs to be specified.                                                --

        if (nrow(RRMSEDSSTransitionDT) == 1) {
          RRMSEDSSTransitionDT[,  c(as.character(0:9)) := as.list(
                                   Reduce(`+`, .SD, accumulate = TRUE)),
                                  .SDcols = c(as.character(0:9))]
  
        } else {
  
          RRMSEDSSTransitionDT[, c(as.character(0:9)) :=
                         Reduce(`+`, .SD, accumulate = TRUE),
                        .SDcols = c(as.character(0:9))]
        }

    
      # Melt long so that each individual has a column vector of each EDSS    --
      # 'To' and the cumulative probability for that 'To'.                    --
  
        RRMSEDSSTransitionDT <-
         melt(RRMSEDSSTransitionDT[, c('interventionID', 'parameterSetID', 'RRMSEDSSTransitionOutcomeRandNo',
                                        as.character(c(0:9)))],
                id.vars = c('interventionID', 'parameterSetID', 'RRMSEDSSTransitionOutcomeRandNo'))


      # Set all 0 values to negative values so that the random number   --
      # can't be < them regardless of floating point errors             --

        RRMSEDSSTransitionDT[value == 0, value := -1]


      # Order
        
        setorder(RRMSEDSSTransitionDT, interventionID, parameterSetID, variable)


      # Select first row where the random number is smaller than 'value'      --
      # with appropriate precision                                            --

        RRMSEDSSTransitionDT <-
         RRMSEDSSTransitionDT[(RRMSEDSSTransitionOutcomeRandNo - value) < 1e-10,
                             .SD[1],
                           by = .(interventionID, parameterSetID)]

        RRMSEDSSTransitionDT[, variable := as.integer(as.character(variable))]



      # Merge with RRMS subset on id into current cycle RRMS EDSS             --

        RRMSPopulation[RRMSEDSSTransitionDT,
                    c(currentCycleEDSSCol) := i.variable,
                    on = .(interventionID, parameterSetID)]

      # Tidy and return

        remove(RRMSEDSSTransitionDT)
    
        RRMSPopulation[, RRMSEDSSProgressionDMTEffect := NULL]    
      
        return(RRMSPopulation)
  
        
}

# ==============================================================================
