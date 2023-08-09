#==============================================================================#
#==============================================================================#
#                                                                              #
#---                      Non-specific DMT Processing                       ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
# Splits non-specific DMTs on the basis of clinical activity and intolerance,  #
# assigning current DMT to continue for the next cycle in those without        #
# disease activity, and returning separated clinical activity switch and       #
# intolerant switch populations.                                               #
#                                                                              #
# ============================================================================ #


otherDMTsProcessing <- function(population, nextCycleDMTColname, cycleNum){
  
  
  # Measure population to check matching incoming/outgoing
  
    popSize <- nrow(population)
    

  # Split on clinical activity
  
    switch_clinicalActivity <- population[clinicalActivity > 0]
    
    population <- population[clinicalActivity == 0]
    

  # Determine intolerance and split intolerants
  
    population <- determineIntolerance(population)
    
    switch_intolerance <- population[DMTIntolerance == 1]
    
    continuing <- population[is.na(DMTIntolerance)]
    
    
  # For those not intolerant and with good clinical response, continue 
  # and increment counter
  
    continuing[, c(nextCycleDMTColname) := 
                 currentCycleDMT]
   
    
  # Create list to return and check incoming/outgoing population sizes
  
    returnList <- list(otherDMTsSwitch_clinicalActivity = switch_clinicalActivity,
                       otherDMTsSwitch_intolerance = switch_intolerance,
                       otherDMTs_continuing = continuing)
  
    checkSize <- sum(sapply(returnList, nrow))
    
        
    if (abs(popSize - checkSize) >= 1){
      
      stop('!!! Incoming and outgoing DMT population size mismatch. Aborted. !!!')
    }
   
    
  # Return 
    
    return(returnList)
    
}