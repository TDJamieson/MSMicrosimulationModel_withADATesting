#==============================================================================#
#                                                                              #
#---                  Assign Relapses to those in RRMS                      ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Relapses are assumed only to occur in those in RRMS.  An individual can      #
# experience a number of relapses in each year in a process which is best      #
# modelled using a negative binomial distribution. The parameters required for #
# the negative binomial are based on the mean, and derived from dispersion.    # 
#                                                                              #
# The mean annualised relapse rate is known to vary by onset age, disease      #
# duration and gender. In addition to these it is modified by DMT effect.      #
#
# The specific mean ARR for each individual therefore varies by their          #
# characteristics and DMT effect, which in PSA will be unique for each         #
# individual. In this process, the individual's DMT effect (IRR) is merged in  #
# and multiplied by the ARR for their subset by characteristics, and then      #
# the rnbinom drawn based on that. NB at present a constant shape parameter is #
# assumed for all subsets.                                                     #
#                                                                              #
# ============================================================================ #


assignRelapses <- function(RRMSPopulation = RRMSSubset, 
                           relapseColumn = currentCycleRelapseCol,
                           cycleNum = cycle,
                           lastCycleNum = lastCycle) {
  
 
  # Identify disease duration band in this cycle                              --
      
      RRMSPopulation[, diseaseDuration := cycleNum] 
      
      RRMSPopulation[diseaseDurationBands, diseaseDurationBand := i.Group,
                     on = .(diseaseDuration >= Min , diseaseDuration <= Max)]
  
      
       
  # ----------------------------------------------------------------------------       
  # Merge in individualised DMT effect for the DMT an individual was on at    --
  # the end of the last cycle. The DMTARREffectsDT table holds the values for   -- 
  # this effect for each DMT for each individual.  This varies depending on   --
  # whether or not varying DMT effect by cycle is desired and has been set    --
  # in the controls spreadsheet.  If it has been set, then the table contains --
  # an effect for each individual for each cycle, either individualised or at --
  # parameter set level.  If varying DMT effects by cycle has not been set    --
  # then the table contains only one entry with one DMT effect for the whole  --
  # of the microsimulation.  In this case since merging is done for each      --
  # cycle then the name of the field containing the DMT effect must be        --
  # updated for each new cycle.                                               --
  # 
  # Also merge in annualised relapse rate 
  # ------------------
   

        if(varyDMTEffectByCycle_Switch == 0){
          
          setnames(DMTARREffectsDT, c(paste0('DMTARREffect_Cycle', lastCycleNum)),
                   c(paste0('DMTARREffect_Cycle', cycleNum)))
          
        }

        
        if(PSA_Switch == 1 & individualLevelPSA_probabilisticSwitch == 1){
  
          # ARR
          RRMSPopulation[annualRelapseRiskGrid, 
                         ARR := i.estimate,
                         on = .(interventionID, parameterSetID, onsetAgeBand, 
                                femaleGender, sensoryOnset, diseaseDurationBand)]
          
          # DMT
          RRMSPopulation[DMTARREffectsDT[, .(interventionID, dmtID, parameterSetID,
                                            ARRDMTEffect = get(paste0('DMTARREffect_Cycle', cycleNum)))],
                        ARRDMTEffect := i.ARRDMTEffect,
                        on = .(interventionID, parameterSetID, currentCycleDMT = dmtID)]
          
          
        } else {
          
          # ARR
          RRMSPopulation[annualRelapseRiskGrid, 
                         ARR := i.estimate,
                         on = .(parameterSetID, onsetAgeBand, femaleGender, 
                                sensoryOnset, diseaseDurationBand)]
          
          # DMT
          RRMSPopulation[DMTARREffectsDT[, .(dmtID, parameterSetID, interventionID,
                                           ARRDMTEffect = get(paste0('DMTARREffect_Cycle', cycleNum)))],
                        ARRDMTEffect := i.ARRDMTEffect,
                        on = .(interventionID, parameterSetID, currentCycleDMT = dmtID)]
          
        } 
        
 
    # Remove DMT effects from DMT DT for memory efficiency if varying by cycle

       if(varyDMTEffectByCycle_Switch == 1){
        
        DMTARREffectsDT[, paste0('DMTARREffect_Cycle', cycle) := NULL]

      }
        
        
  # ----------------------------------------------------------------------------
   
       
 
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      print(paste0('Assigning Relapses to ', 
             nrow(RRMSPopulation), 
           ' remaining in RRMS'))
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  
 
  # Multiply individual ARR by DMTEffect                                      --
                
      RRMSPopulation[, relapseRisk := 
                        ARR * ARRDMTEffect]
      
      
  # Since the distribution of relapses is fundamentally important to modelling -
  # options are built in here:
  
  # Negative binomial
    
    if (relapseDistribution == 'negativeBinomial') {
  
      
      # Generate rnbinom size parameter using conditional variance with      --
      # rescaled ARR                                                          --
      
        RRMSPopulation[, varCondOnMean := scale * relapseRisk]
        
        RRMSPopulation[, rnbinomSize := (relapseRisk + relapseRisk^2)/
                                          varCondOnMean]
      
      # Draw from rnbinom for each individual using their unique mean ARR     --
    
        RRMSPopulation[, c(relapseColumn) := qnbinom(ARROutcomeRandNo,
                                             size = rnbinomSize,
                                             mu = relapseRisk)]
        
        if(is.na(sum(RRMSPopulation[, get(relapseColumn)]))) browser()
      
    }
      
      
  # Poisson 
  # 
    if (relapseDistribution == 'Poisson') {
      
      # Draw from rpois for each individual using their unique mean ARR       --
       
         RRMSPopulation[, c(relapseColumn) := qpois(ARROutcomeRandNo,
                                            lambda = relapseRisk)]
         
        if(is.na(sum(RRMSPopulation[, get(relapseColumn)]))) browser()    
    }

      
      
  # Tidy                                                                      --
      
      suppressWarnings(
      RRMSPopulation[, `:=` (relapseRisk = NULL,
                             rnbinomSize = NULL,
                             diseaseDurationBand = NULL,
                             diseaseDuration = NULL,
                             varCondOnMean = NULL,
                             scale = NULL)]
      )

  # Outcome checks *************************************************************
    
    relapseOutcomesDT <- 
      RRMSPopulation[, .(noRelapses = sum(get(relapseColumn), na.rm = TRUE),
                   noIndividuals = as.integer(.N),
                   Cycle = cycleNum),
                  by = .(onsetAge, femaleGender, sensoryOnset, seedGroup,
                         parameterSetID, interventionCohort)]
    
    relapseOutcomesDT[, relapseRate := noRelapses/noIndividuals, 
             by = .(onsetAge, femaleGender, sensoryOnset, seedGroup,
                    parameterSetID, interventionCohort)]
    
    
 # *****************************************************************************                       
  
    
  # Tidy and Return 
  
    RRMSPopulation[, c('ARR', 'ARRDMTEffect') := NULL]

      return(list(RRMSPopulation = RRMSPopulation,
             relapseOutcomesCheck = relapseOutcomesDT))

}
     
# ==============================================================================
         