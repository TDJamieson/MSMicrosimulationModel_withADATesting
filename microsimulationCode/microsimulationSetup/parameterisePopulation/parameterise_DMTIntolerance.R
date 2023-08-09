#==============================================================================#
#                            DMT intolerance                                   #
#===============================================================================
#                                                                              #
# This uses a beta distribution and input values created in inputs_prepare.R   #
# to create a single risk of intolerance to each DMT.                          #
#                                                                              #
# ============================================================================ #
  

    # Create DT with an entry for each parameter set
    
      DMTIntoleranceRisksDT <- as.data.table(
        expand_grid(DMTIntoleranceRisks[, .(dmtID, shape1, shape2)], 
                    parameterSetID = unique(MSSample[, .(parameterSetID)]))
      )
      setDT(DMTIntoleranceRisksDT)
      
   
    # If probabilistic switch on then create DT full of samples drawn from 
    # beta distribution using parameters derived above
        
        if (PSA_Switch == 1) {
          
          if (parameterSetLevelPSA_probabilisticSwitch == 1){
         

              DMTIntoleranceRisksDT[, DMTIntoleranceRisk := 
                                      
                                      {dqset.seed(parameterSetID, NULL)
                
                                        dqrunif(seedRunN + dmtID)

                                        rep(qbeta(dqrunif(1), 
                                                  shape1 = shape1,
                                                  shape2 = shape2),
                                                  .N)},
                   
                                    by = .(dmtID, parameterSetID)]
                
              
          } else if (individualLevelPSA_probabilisticSwitch == 1){  
            
            
            stop("Individual level PSA not implemented for DMT intolerance")
            
          }
          
        } else {
          
          DMTIntoleranceRisksDT <- 
            as.data.table(
                expand_grid(DMTIntoleranceRisks[, .(dmtID, DMTIntoleranceRisk = rate_Intolerance)], 
                            unique(MSSample[, .(parameterSetID)]))
          )
        
        }
              
              
      # For placebo assign intolerance risk of 0
        
        DMTIntoleranceRisksDT[dmtID == 0, DMTIntoleranceRisk := 0]
        
      
      # Increment seed run

        seedRunN <<- seedRun_DMTIntolerance   

        
#===============================================================================
 