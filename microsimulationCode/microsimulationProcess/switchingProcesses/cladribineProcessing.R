#==============================================================================#
#==============================================================================#
#                                                                              #
#---                         Cladribine Switching                           ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
# Cladribine is given as two doses a year apart and then discontinued with the #
# assumption that it's ongoing clinical effect is the same as during years in  #
# which it is being given. The processes needed are therefore:                 #
#                                                                              #
#  1. Year 1  - Since they are on active drug test for intolerance, if         #
#              intolerant mark for switching                                   #
#             - Test for clinical activity - if at the end of the year they    #
#              have ongoing clinical activity then switch                      #
#             - If no intolerance and clinically responsive then               #
#              continue to year 2 dose                                         #
#                                                                              #
#  2. Year 2  - Since they have had second dose don't test for intolerance     #
#             - Test for clinical activity and mark for switching if           #
#               response is suboptimal                                         #                                      
#             - Move to course 'complete'                                      #
#                                                                              #
#  3. Year 3+ - No intolerance testing                                         #
#             - Test for clinical activity, if no longer responsive mark       #
#               for switching otherwise continue as course 'complete'          #
#                                                                              #
# ============================================================================ #


cladribineProcessing <- function(population, nextCycleDMTColname,
                                 cycleNum){
 
  # Split populations ----------------------------------------------------------
  
    # Check no in population to ensure incoming outgoing are the same #
  
        popSize <- nrow(population)
 
          
    # Split into years
  
        firstYearPopulation <- population[currentDMTDoses == 1]
        subsequentYearsPopulation <- population[currentDMTDoses >= 2]
      
  # ----------------------------------------------------------------------------

      
     
  # Year 1 processing ----------------------------------------------------------    
       
  # Identify those with ongoing clinical activity in year 1 for switching 
  # and split off
  
      firstYearPopulationSwitch_clinicalActivity <-
        firstYearPopulation[clinicalActivity > 0]
      
      firstYearPopulation <- firstYearPopulation[clinicalActivity == 0]
      
      
  # For those not switching for clinical activity determine intolerance and 
  # split off intolerants

      firstYearPopulation <- determineIntolerance(firstYearPopulation)
      
      firstYearPopulationSwitch_intolerance <- 
        firstYearPopulation[DMTIntolerance == 1]
      
      firstYearPopulation <- 
        firstYearPopulation[is.na(DMTIntolerance)]
    
      
  
  # Now in those with an adequate clinical response in year 1
  # continue to year 2 dose and increment time on DMT counter
  
    firstYearPopulation_continuing <-
      firstYearPopulation[, c(nextCycleDMTColname) := cladribineID]
   
    firstYearPopulation_continuing[, currentDMTDoses := 
                                     currentDMTDoses + 1]
    
  # ----------------------------------------------------------------------------
  
    
    
  # Year 2 processing ----------------------------------------------------------
    
  # Now identify responders/non-responders in year 2 and beyond and 
  # assign continuing 'complete' course to responders and increment 
  # time on DMT
    
    subsequentYearsPopulation_continuing <- 
      subsequentYearsPopulation[clinicalActivity == 0]
    
    subsequentYearsPopulation_continuing[, c(nextCycleDMTColname) := 
                                cladribineCompleteID]
 
    subsequentYearsPopulationSwitch_clinicalActivity <- 
      subsequentYearsPopulation[clinicalActivity > 0]
    
    
  # ----------------------------------------------------------------------------
    
    
  # Test incoming and outgoing population are the same size and then return
  
    returnList <- 
      list(cladribineSwitch_intolerance = firstYearPopulationSwitch_intolerance, 
       
           cladribineSwitch_clinicalActivity = rbind(firstYearPopulationSwitch_clinicalActivity,
                                                     subsequentYearsPopulationSwitch_clinicalActivity),
           cladribine_continuing = rbind(firstYearPopulation_continuing,
                                         subsequentYearsPopulation_continuing))
  
    
    checkSize <- sum(sapply(returnList, nrow))
    
    
    if(abs(popSize - checkSize) >= 1){
      
      stop('!!! Incoming/outgoing cladribine population sizes do not match. Aborted. !!!')
      
    }
    
  
  # Return list of ongoing, intolerance switch and clinical activity switch
    
    return(returnList)
  
}
