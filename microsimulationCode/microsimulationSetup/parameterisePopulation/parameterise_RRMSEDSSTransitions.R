#==============================================================================#
#                    Parameterise RRMS EDSS transitions                        #
#===============================================================================
#                                                                              #
# RRMS transitions are EDSS dependent, the matrix used has a transition        #
# probability for each EDSS.  For base case analysis these values are          #
# replicated across all individuals and the same matrix taken in for all       #
# cycles.                                                                      #
#                                                                              #
# For PSA, only parameter set level variability is currently possible.         #
# multidir, the dirichlet generating package allows only one seed  - the       #
# PSA ID is used to set the seed.                                              #
#                                                                              #
# Since their are two transition matrices - for above and below 28 onset,      #
# the process is repeated for each with some wasting of runifs before the      #
# second to avoid getting the same underlying random numbers generating        #
# both distributions.                                                          #
#                                                                              #
# Whether the base case or PSA distributions have been generated, the          #
# probabilities are converted to rates since this is what DMT effects are      #
# applied to and you would otherwise be repeating that process wastefully in   #
# each microsimulation cycle.                                                  #
#                                                                              #
#==============================================================================#  

  # ----------------------------------------------------------------------------
  # Copy matrices to preserve for microsimulation cycling
  # ------
  
    transitionMatrix_EDSS_RRMSDT <- 
      transitionMatrix_EDSS_RRMS
    
    dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT <- 
      dirichlet_transitionMatrix_EDSS_RRMS_over28Onset
    
    dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT <- 
      dirichlet_transitionMatrix_EDSS_RRMS_under28Onset 

  # ----------------------------------------------------------------------------

    
    
  # ----------------------------------------------------------------------------
  # Base case 
  # ----------

  if (EDSSTransitions_probabilisticSwitch == 0){
    
    # Assign mean transition matrix values to all parameter sets
  
      transitionMatrix_EDSS_RRMSDT <- as.data.table(expand_grid(
          unique(MSSample[, .(parameterSetID)]), 
          transitionMatrix_EDSS_RRMSDT
      ))

  }
    
  # ----------------------------------------------------------------------------
      
      
       
  # ----------------------------------------------------------------------------      
  # For PSA use Dirichlet distribution to create individual or parameter set
  # matrices
  # ----

      if (EDSSTransitions_probabilisticSwitch == 1){
        
        
        if(individualLevelPSA_probabilisticSwitch == 1){
          
          stop('EDSS transition PSA not implemented yet')
          
        } else if (parameterSetLevelPSA_probabilisticSwitch == 1){
          
          
          
      # Remove 'From columns from transition matrices so these don't contribute 
      # to Dirichlet draws
      
        dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT[, From := NULL]
        dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT[, From := NULL]

    
        
      # Add 1 to all cells in matrix of transition numbers to create 
      # non-zero transition probabilities in all cells as per Briggs, Ades
      # and Price (2003)
        
        dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT <- 
          dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT + 1
        dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT <- 
          dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT + 1
        
      
      # Draw from dirichlet for each row in matrix of transition numbers
      # for each PSA seed (doesn't use dqrng, base seed) - under 28
      
      transitionMatrix_EDSS_RRMS_under28 <-
        
          as.data.table(
            do.call(rbind,
        
              lapply(as.list(unique(MSSample[, parameterSetID])), 
                     
                     function(PSAID) {
 
                        set.seed(PSAID)
                      
                        DT <- as.data.table(do.call(rbind,
                  
                  
                        lapply(1:nrow(dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT),
                       
                               function(row){
                         
                                  rdirichlet(1, 
                                             as.matrix(dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT)[row,])
                       
                        })
                      
                  ))
            
            
                colnames(DT) <- as.character(c(0:9))
                DT[, parameterSetID := PSAID]
                DT[, under28 := 1]
            
           
                DT <- cbind(DT, From = c(0:9))
         
                 })
            )
          )
      
      
      # Draw from dirichlet for each row in matrix of transition numbers
      # for each PSA seed (doesn't use dqrng, base seed) - over 28
      
      transitionMatrix_EDSS_RRMS_over28 <-
        
          as.data.table(
            do.call(rbind,
        
              lapply(as.list(unique(MSSample[, parameterSetID])), 
                     
                     function(PSAID) {
           
                      # Need to waste some runifs to avoid getting same 
                      # underlying draw as above
                      set.seed(PSAID)
                      runif(1000)
                      
                      DT <- as.data.table(do.call(rbind,
              
              
                      lapply(1:nrow(dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT),
                   
                            function(row){
                     
                              rdirichlet(1, 
                                         as.matrix(dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT)[row,])
                       
                   })
                  
                  ))
            
            
                colnames(DT) <- as.character(c(0:9))
                DT[, parameterSetID := PSAID]
                DT[, under28 := 0]
            
           
                DT <- cbind(DT, From = c(0:9))
         
                 })
            )
          )
      
      
      
      # Combine the two sets of transition matrices and tidy 
      
        transitionMatrix_EDSS_RRMSDT <- rbind(transitionMatrix_EDSS_RRMS_over28,
                                             transitionMatrix_EDSS_RRMS_under28)
        setcolorder(transitionMatrix_EDSS_RRMSDT, 'From')
        
        remove(dirichlet_transitionMatrix_EDSS_RRMS_over28OnsetDT,
               dirichlet_transitionMatrix_EDSS_RRMS_under28OnsetDT,
               transitionMatrix_EDSS_RRMS_over28,
               transitionMatrix_EDSS_RRMS_under28)
      
     
      }
      
    
  }
    
  # ----------------------------------------------------------------------------    
   
    
      
  # ----------------------------------------------------------------------------
  # Convert probabilities to rates to allow DMT effects to be applied
  #------
    
   # Identify fields that need to be transformed                              --
      
      EDSSValues <- as.character(0:9)
    
    
    # Use data.table set function to transform each EDSS field using          --
    # probability to rate function defined in defineFunctions.R               --
    
      # RRMS EDSS Progression transition matrix
        
        for(j in EDSSValues){
                
          set(transitionMatrix_EDSS_RRMSDT, j=j,
              value = (probabilityToRate(
                transitionMatrix_EDSS_RRMSDT[[as.character(j)]]
                )
              )
          )}
      
      transitionMatrix_EDSS_RRMS_Rates <- transitionMatrix_EDSS_RRMSDT
      
      remove(transitionMatrix_EDSS_RRMSDT)
  
          
  # ----------------------------------------------------------------------------
