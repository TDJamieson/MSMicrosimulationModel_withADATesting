#==============================================================================#
#                                                                              #
#---              Produce long format microsimulation outcome               ---#
#                                                                              #
#==============================================================================# 

#  In order to assign costs and utilities by cycle a long format dataset is    #
#  needed for merging in appropriate values.  This function takes all the      #
#  relevant types of outcomes, melts them and joins them together.             #
#
# ============================================================================ #


wideToLongMicrosimulationOutcome <- function(microsimulationDT = microsimulationOutcome){
  
  # Define function to take a type of outcome defined by a pattern in a 
  # variable name, ane melt to give a row with outcome for each person for 
  # each cycle
  
      meltSpecific <- function(DT, fieldPattern, newVarName){
         
        fields <- colnames(DT)[grepl(colnames(DT),
                                pattern = fieldPattern)]
        
        microsimulationTraces <-
          DT[, c('interventionID', 'parameterSetID', ..fields)]
        
        setnames(microsimulationTraces,
                 old = c(fields),
                 new = gsub(fieldPattern, "", fields))
        
        
        microsimulationEDSSTraces <- melt(microsimulationTraces,
                                          id.vars = c('interventionID', 'parameterSetID'),
                                          variable.name = "Cycle",
                                          value.name = newVarName)
        
      }
      
  
  # Melt all desired fields in turn
      
    EDSS <- meltSpecific(microsimulationDT, "^EDSS_Cycle", 'EDSS')
    
    MSType <- meltSpecific(microsimulationDT, "^MSType_Cycle", 'MSType')
    
    RRMSType <- meltSpecific(microsimulationDT, "^RRMSType_Cycle", 'RRMSType')
    
    DMT <- meltSpecific(microsimulationDT, "^DMT_Cycle", 'DMT')
    
    relapses <- meltSpecific(microsimulationDT, "^Relapses_Cycle", 'Relapses')
    
    
  # Trim off any cycles where an indvidual is dead from EDSS melt and use this 
  # smaller DT to merge onto 
    
    EDSS <- EDSS[!is.na(EDSS)]
    
    meltedTrace <- Reduce(merge, list(EDSS, MSType, RRMSType, DMT, relapses))
    
    meltedTrace[, Cycle := as.integer(Cycle)]
    
    
  # Add age in each cycle
    
    meltedTrace[microsimulationDT[, .(seedGroup, interventionID, parameterSetID, startingAge)],
                startingAge := i.startingAge,
                on = .(interventionID, parameterSetID)]
    
    meltedTrace[, currentAge := startingAge + Cycle - 1]
    
  
  # Add seedGroup and personID
    
    meltedTrace[microsimulationDT[,.(interventionID, seedGroup, personID)], 
                `:=` (seedGroup = i.seedGroup, personID = i.personID),
                on = .(interventionID), 
                mult = 'all']
    
  
}

