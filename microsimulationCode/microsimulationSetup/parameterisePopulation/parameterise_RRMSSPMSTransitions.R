#==============================================================================#
#                    Parameterise RRMS to SPMS transitions                     #
#===============================================================================
#                                                                              #
# SPMS transitions are EDSS dependent, the matrix currently in use has a       #
# transition probability for each EDSS, with EDSS destination of one value     #
# higher. For base case analysis these values a replicated across all          #
# individuals for all cycles.                                                  #
#                                                                             #
# For PSA...
#==============================================================================#  

  # Copy transitionMatrix_RRMStoSPMS to preserve for subsequent cycling
  
     transitionMatrix_RRMStoSPMSDT <- copy(transitionMatrix_RRMStoSPMS)


  # ----------------------------------------------------------------------------
  # Base case 
  # ----------
  

    # The current matrix has a single entry for each EDSS with only a 
    # probability for transiting to an EDSS one state higher, implying that
    # the remaining probability is of remaining in the current 'From' EDSS.
    # This can be formalised by assigning that 1- the probability of 
    # transition.
    

      transitionMatrix_RRMStoSPMSDT[transitionMatrix_RRMStoSPMSDT == 0] <- NA
      transitionMatrix_RRMStoSPMSDT[is.na(From), From := '0']
  
      transitionMatrix_RRMStoSPMSDT <- melt(transitionMatrix_RRMStoSPMSDT, 
                                          measure = list(as.character(0:9)), 
                                          id.vars = 'From')
      setorder(transitionMatrix_RRMStoSPMSDT, From, variable)
      transitionMatrix_RRMStoSPMSDT[, nextVal := shift(value, -1), by = From]
      transitionMatrix_RRMStoSPMSDT[!is.na(nextVal), value := 1-nextVal]
      transitionMatrix_RRMStoSPMSDT[, nextVal := NULL]
      transitionMatrix_RRMStoSPMSDT <- dcast(transitionMatrix_RRMStoSPMSDT, 
                                           formula = From ~ variable)
      
      transitionMatrix_RRMStoSPMSDT[is.na(transitionMatrix_RRMStoSPMSDT)] <- 0

   
   
    # Assign mean transition matrix values to all (nb not by cycle here)
  
       transitionMatrix_RRMStoSPMSDT <- 
         as.data.table(expand_grid(unique(MSSample[, .(parameterSetID)]), 
                                   transitionMatrix_RRMStoSPMSDT)
                        )
    
  # ============================================================================
      
      
   
    
 # ----------------------------------------------------------------------------
 # PSA
 # ----------
       
      
    seedRunN <<- seedRun_RRMSSPMStransitions
       
       
 # ============================================================================
       
