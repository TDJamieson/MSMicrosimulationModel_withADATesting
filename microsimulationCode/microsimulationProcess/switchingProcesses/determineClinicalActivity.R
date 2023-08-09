#==============================================================================#
#==============================================================================#
#                                                                              #
#---                     Determine clinical activity                        ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
# This takes a population of RRMS individuals and returns them with their      #
# clinical activity defined - absent, ongoing, or RES; if they have RES or     #
# ongoing then they are marked as for switching. If they enter RES then there  #
# is a separate RES switch which persists once it takes a value of 1 so that   #
# people don't move to a lower level treatment strategy from a higher one.     #
#                                                                              #
#                                                                              #
# Definitions:                                                                 #  
#                                                                              #  
# Absent clinical activity: Zero relapses                                      #
#                                                                              #
# Ongoing clinical activity: One relapse                                       #
#                                                                              #
# Rapidly Evolving Severe: Two or more relapses                                #
#                                                                              #
# NB in this generic model switching is based solely on clinical relapses,     #
# not on MRI, though this contributes significantly in clinical practice       #
# including in the definitions of activity.                                    #
#                                                                              #
# ===========================================================================  # 


determineClinicalActivity <- function(population, currentCycleRelapseCol) {
  
  
  # Determine number of relapses then assign clinicalActivty - 
  # 0: No activity
  # 1: Ongoing (single relapse)
  # 2: RES (>1 relapse)
  # 
  # Mark 'switch' with 1 for clinical activity switching
  # Mark RES with 1 for those with >1 relapse
  
  population[get(currentCycleRelapseCol) == 0, clinicalActivity := 0L]
  
  population[get(currentCycleRelapseCol) == 1, `:=` (clinicalActivity = 1L,
                                                     switch = 1L)]
  
  population[get(currentCycleRelapseCol) > 1, `:=` (clinicalActivity = 2L,
                                                    RRMSType = 'RES',
                                                    switch = 1L)]
  
  return(population)

  
}
