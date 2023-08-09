#==============================================================================#
#                                                                              #
#---                        Assign relapse types                            ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# This function takes a population and if any have experienced relapse(s) in   #
# this cycle it determines what type of relapse they have had as set out in    #
# the input matrices spreadsheet. NB the three types of relapse are hard coded #
# here so there is little flexibility to change simple by changing the         #
# spreadsheet.                                                                 #
#                                                                              #
#==============================================================================#  


assignRelapseTypes <- function(population = alivePopulation,
                               relapseColumn = currentCycleRelapseCol) {
  
  
# Progress output *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
   print('Assigning relapses')
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

  
  # If any individuals had a relapse in this cycle then determine type(s)     --
  # since cost and utility depend on this                                     --
  
  if (nrow(population[get(relapseColumn) > 0]) > 0){


    # Select abbreviated DT for those with relapse(s), rename for convenience --
      
      relapseTypes <- population[get(relapseColumn) > 0, 
                                 c('interventionID', ..relapseColumn)]
      setnames(relapseTypes, old = relapseColumn, new = 'relapses')
      
      
    # Create DT with number of rows corresponding to number of relapses for   --
    # each individual                                                         --
    
      relapseTypes <- 
        relapseTypes[rep(1:.N,relapses)][,Indx:=1:.N,by=interventionID]
      
 
    # Now sample from relapse-subtypes table, using typical approach of       --
    # runif(0,1), melt, and take from bucket where runif falls                --
     
      
    # Produce cumulative probabilities for subtypes in subtypes tables        --
    
      relapse_subtypesTable <- copy(relapse_subtypes)
      relapse_subtypesTable[, probability := cumsum(Proportion)]
      
      
    # Add random numbers to choose bucket                                     --
      
      relapseTypes[, relapseRandNo := runif(.N)] 
      
      
    # Expand_grid so that each type of relapse is replicated for each relapse --
    # for each individual
    
      relapseTypes <- as.data.table(
        expand_grid(relapseTypes, relapse_subtypesTable)
      )

      
    # Order 
    
      setorder(relapseTypes, interventionID, Indx, probability)
      
      
    # Take first record where runif falls in bucket

      relapseTypes <- relapseTypes[relapseRandNo <= probability, .SD[1], 
                                   by = .(interventionID, Indx)]
      
      
    # Return 
      
      return(relapseTypes)
      
}

}
