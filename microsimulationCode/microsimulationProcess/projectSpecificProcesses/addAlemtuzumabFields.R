#==============================================================================#
#                                                                              #
#---   Alemtuzumab-specific variables to add to microsimulation process     ---#
#                                                                              #
#==============================================================================# 

# These are additional variables specific to Alemtuzumab which for clarity     #
# are handled outside of the main, mostly generic, microsimulation process     #
#                                                                              #
# ============================================================================ #


  # Fields related to alemtuzumab ADAs --

    workingSample[, `:=` (alemtuzumabADAs = NA_integer_,
                          ADAsTested = NA_integer_,
                          ADATestingCycle = NA_integer_,
                          alemtuzumabTestOutcome = NA_character_)]
  
  
  # Field to mark if an individual was treated with alemtuzumab --
  
    workingSample[, alemtuzumabTreated := NA_integer_]
    
    
  # Field to mark if alemtuzumab would have been switched had ADA testing --
  #  been done                                                            --
    
    workingSample[, wouldHaveSwitched := NA_integer_]
    
    

  # Add an alemtuzumab doses and time on calculator --
    
    workingSample[, alemtuzumabDoses := NA_integer_]
    workingSample[DMT_Cycle0 == alemtuzumabID,
                  alemtuzumabDoses := 1]
    workingSample[DMT_Cycle0 == alemtuzumabID,
                  timeOnAlemtuzumab := 1]
    
  
  # Define names of alemtuzumab fields
    
    alemtuzumabFields <- c('alemtuzumabADAs', 'ADAsTested', 'ADATestingCycle',
                           'alemtuzumabTestOutcome', 'alemtuzumabTreated', 
                           'wouldHaveSwitched', 'alemtuzumabDoses',
                           'timeOnAlemtuzumab')
    
# ============================================================================== 