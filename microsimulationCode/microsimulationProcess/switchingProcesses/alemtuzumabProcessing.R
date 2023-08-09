#==============================================================================#
#==============================================================================#
#                                                                              #
#---                         Alemtuzumab Processing                         ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
# Alemtuzumab is given as two doses a year apart as the initial 'course'.      #
# A third dose may be given for suboptimal response, either after the second   #
# dose or subsequently. Processes depend therefore on year:                    #
#                                                                              #
#  1. Year 1  - Since they are on active drug test for intolerance, if         #
#              intolerant mark for switching                                   #
#             - If no intolerance and clinically responsive then               #
#              continue to year 2 dose                                         #
#                                                                              #
#  2. Year 2  - Test for intolerance                                           #
#             - Test for clinical activity and mark for switching if           #
#               response is suboptimal                                         #                                      
#             - Move to course 'complete'                                      #
#                                                                              #
#  3. Year 3+ - No intolerance testing                                         #
#             - Test for clinical activity, if no longer responsive mark       #
#               for switching otherwise continue as course 'complete'          #
#                                                                              #
# ============================================================================ #

  alemtuzumabProcessing <- function(population = alemtuzumabPopulation,
                                    nextCycleDMTColname = nextCycleDMTColumn,
                                    cycleNum) {
 
     
  # Split populations ----------------------------------------------------------
 
    # Note population size to check incoming/outgoing
    
        popSize <- nrow(population)
 
        
    # Define empty DT to avoid rbind problems
    
        emptyDT <- population[0]
    
    
    # Split population 
  
        postDoseOnePopulation <-
          population[alemtuzumabDoses == 1]
        
        postDoseTwoPopulation <- 
          population[alemtuzumabDoses == 2]
        
        postDoseThreePopulation <- 
          population[alemtuzumabDoses == 3]
  
        
  # ----------------------------------------------------------------------------
  
        
        
  # Post dose 1 processing -----------------------------------------------------
    
    # For Year 1 population determine intolerance but don't assess
    # clinical activity, if no intolerance continue to second dose, increment 
    # time on current DMT and increment alemtuzumab dose counter
          
        if(nrow(postDoseOnePopulation) > 0) {
        
          postDoseOnePopulation <- determineIntolerance(postDoseOnePopulation)
          
          
        # Add cycle number in which to test for autoimmune thyroid disease
          postDoseOnePopulation[, alemtuzumabThyroidTestingCycle := 
                                             cycleNum + 2]
    
                
        # Split based on intolerance
    
          postDoseOnePopulationSwitch_intolerance <- 
            postDoseOnePopulation[DMTIntolerance == 1]
          
          postDoseOnePopulation_continuing <- 
            postDoseOnePopulation[is.na(DMTIntolerance)]
          
          
        # Allocate alemtuzumab dose 2 ID to those who are continuing, increment
        # generic dose counter and alemtuzumab dose counter
          
          postDoseOnePopulation_continuing[, c(nextCycleDMTColname) := alemtuzumabDose2ID]
          postDoseOnePopulation_continuing[, currentDMTDoses := currentDMTDoses + 1]
          postDoseOnePopulation_continuing[, alemtuzumabDoses := alemtuzumabDoses + 1]
          
      
        # All who have dose 2 are tested for ADAs, currently assumed to be 
        # essentially contemporaneous with administration
        
          postDoseOnePopulation_continuing <- 
                testAlemtuzumabADAs(postDoseOnePopulation_continuing)
          
          
         
        } else { # If there isn't anybody in the post dose one population
          
          postDoseOnePopulationSwitch_intolerance <- emptyDT
          postDoseOnePopulation_continuing <- emptyDT
       
        }
        
        
  # ----------------------------------------------------------------------------
  
        
        
  
  # Post dose 2 processing -----------------------------------------------------
        
    # For year 2 population test for clinical activity - if ongoing they 
    # are allowed third dose, but intolerance should be tested for first
            
        if(nrow(postDoseTwoPopulation) > 0) {
          
 
          # If no disease activity, continue assign 'alemtuzumabCourse1CompleteID'
          
            postDoseTwoPopulation_course1Complete <- 
                postDoseTwoPopulation[clinicalActivity == 0]
              
              
            postDoseTwoPopulation_course1Complete[, c(nextCycleDMTColname) :=
                                                    alemtuzumabCourse1CompleteID]
              
        
          # If ongoing activity test for intolerance and place in switching
          # group if intolerant
           
              postDoseTwoPopulationOngoingActivity <- 
                postDoseTwoPopulation[clinicalActivity > 0]
          
              postDoseTwoPopulationOngoingActivity <- 
                   determineIntolerance(postDoseTwoPopulationOngoingActivity)
          
              postDoseTwoPopulationSwitch_intolerance <- 
                  postDoseTwoPopulationOngoingActivity[DMTIntolerance == 1]
        
              
          # Keep hold of those not intolerant then apply ADA test outcomes  
              
              postDoseTwoPopulationOngoingActivity <- 
                postDoseTwoPopulationOngoingActivity[is.na(DMTIntolerance)]
              
               
          # Look at ADA levels, but only act on in intervention group
          
              postDoseTwoPopulationOngoingActivityADAPositive_Intervention <- 
                  postDoseTwoPopulationOngoingActivity[(alemtuzumabTestOutcome == 'TP'|
                                                        alemtuzumabTestOutcome == 'FP') & 
                                                         interventionCohort == 1L]
              
              postDoseTwoPopulationOngoingActivityADANegative_Intervention <- 
                postDoseTwoPopulationOngoingActivity[(alemtuzumabTestOutcome == 'TN' |
                                                     alemtuzumabTestOutcome == 'FN') & 
                                                      interventionCohort == 1L ]
              
              postDoseTwoPopulationOngoingActivity_Comparator <- 
                postDoseTwoPopulationOngoingActivity[interventionCohort == 0L]
          
              
          # If no intolerance or ADAs then give 3rd dose, increment time on DMT,
          # and alemtuzumab doses
          
            postDoseTwoPopulation_thirdDose <- 
              rbind(postDoseTwoPopulationOngoingActivityADANegative_Intervention,
                    postDoseTwoPopulationOngoingActivity_Comparator)
                    
          
            postDoseTwoPopulation_thirdDose[, c(nextCycleDMTColname) :=
                                                  alemtuzumabDose3ID]
            
            postDoseTwoPopulation_thirdDose[, currentDMTDoses := 
                                                currentDMTDoses + 1]
            
            postDoseTwoPopulation_thirdDose[, alemtuzumabDoses := 
                                                alemtuzumabDoses + 1]
            
       
         } else {
            
            postDoseTwoPopulation_course1Complete <- emptyDT
            postDoseTwoPopulationOngoingActivityADAPositive_Intervention <- emptyDT
            postDoseTwoPopulationSwitch_intolerance <- emptyDT
            postDoseTwoPopulation_thirdDose <- emptyDT
          
          }
        
        
        
        
  # ----------------------------------------------------------------------------

        
        
  # Post dose 3 processing -----------------------------------------------------

      if(nrow(postDoseThreePopulation) > 0) {
        
        
        # For those who have had 3 doses, switching is only based on clinical
        # activity, so assign those appropriate to clinical switch population
        # and continue alemtuzumab complete for all others
      
        postDoseThreePopulation_courseComplete <- 
                postDoseThreePopulation[clinicalActivity == 0]
        
        postDoseThreePopulation_courseComplete[, c(nextCycleDMTColname) :=
                                                    alemtuzumabCompleteID]
        
        postDoseThreePopulationSwitch_clinicalActivity <- 
                postDoseThreePopulation[clinicalActivity > 0]
        
        
         
      } else {
            
        
        postDoseThreePopulation_courseComplete <- emptyDT
        postDoseThreePopulationSwitch_clinicalActivity <- emptyDT
      
      }
 
 
  # Create list of populations to be returned to higher switching function
        
        returnList <- list(alemtuzumabSwitch_intolerance = 
                             rbind(postDoseOnePopulationSwitch_intolerance,
                                   postDoseTwoPopulationSwitch_intolerance),
                           
                           alemtuzumabSwitch_clinicalActivity = 
                              postDoseThreePopulationSwitch_clinicalActivity,
                           
                           alemtuzumabSwitch_ADAs = 
                             postDoseTwoPopulationOngoingActivityADAPositive_Intervention,
                           
                           alemtuzumab_continuing = 
                             rbind(postDoseOnePopulation_continuing, 
                                   postDoseTwoPopulation_course1Complete,
                                   postDoseTwoPopulation_thirdDose,
                                   postDoseThreePopulation_courseComplete
                                  )
                          )
        
    
    # Check incoming and outgoing population sizes
    
        checkSize <- sum(sapply(returnList, nrow))
        
        if (abs(popSize - checkSize) >= 1){
          
          stop('!!! Incoming and outgoing alemtuzumab population size mismatch. Aborted. !!!')
        }
        
     
    # Return 
    
        return(returnList)
        
        
  }
