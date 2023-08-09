#==============================================================================#
#                    Parameterise SPMS EDSS transitions                        #
#===============================================================================
#                                                                              #
# SPMS transitions are EDSS dependent, For base case analysis these values     #
# are replicated across all individuals and the same matrix taken in for all   #
# cycles.                                                                      #
#                                                                              #
# For PSA, only parameter set level variability is currently possible.         #
# multidir, the dirichlet generating package allows only one seed  - the       #
# PSA ID is used to set the seed.                                              #
#                                                                              #
#==============================================================================#  


  # ----------------------------------------------------------------------------
  # Base case 
  # ----------

    # Assign mean transition matrix values to all (nb not by cycle here)
  
        transitionMatrix_EDSS_SPMSDT <- 
          as.data.table(expand_grid(unique(MSSample[, .(parameterSetID)]), 
                                    transitionMatrix_EDSS_SPMS)
                        )
    
  # ----------------------------------------------------------------------------
      
      
      
       
  # # ----------------------------------------------------------------------------      
  # # For PSA use Dirichlet distribution to create individual or parameter set
  # # matrices
  # # ----
  # 
  #     if (EDSSTransitions_probabilisticSwitch == 1){
  #       
  #       
  #       if(individualLevelPSA_probabilisticSwitch == 1){
  #         
  #         stop('EDSS transition PSA not implemented yet')
  #         
  #       } else if (parameterSetLevelPSA_probabilisticSwitch == 1){
  #         
  #         stop('EDSS transition PSA not implemented yet')
  #         
  #       }
  #         
  #       
  #     }
  #   
  # # ----------------------------------------------------------------------------    
   

    
