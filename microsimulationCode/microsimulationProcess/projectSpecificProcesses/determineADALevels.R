
#==============================================================================#
#                     Determine ADA levels and outcome                         #
#===============================================================================
#                                                                              #
# This takes a population who are on alemtuzumab and determines their ADA      #
# levels, and then determines action based on that.                            #
# ============================================================================ #

# !!!!!!!!!!!!!!!!!!!!!!!!!! Work in Progress !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

  determineADALevels <- function(population = population){
    
       
    population[, ADALevel := runif(.N)]
    population[ADALevel < 0.2, ADALevel := 1L]
    population[ADALevel >= 0.2, ADALevel := NA_integer_]
    
    population[ADALevel == 1, DMTIntolerance := 1L]
    population[is.na(ADALevel), DMTIntolerance := NA_integer_]

    
    # For those not in the testing population, ADA level must be ignored
    
    population[comparatorCohort == 1 & 
                ADALevel == 1, wouldHaveSwitched := 1L]
    population[comparatorCohort == 1, DMTIntolerance := NA_integer_]
    
    population[, ADALevel := NULL]
    
    return(population)

  }