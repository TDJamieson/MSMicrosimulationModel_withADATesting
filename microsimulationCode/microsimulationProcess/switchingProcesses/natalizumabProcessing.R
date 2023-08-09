#==============================================================================#
#==============================================================================#
#                                                                              #
#---                         Natalizumab Processing                         ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
# Natalizumab is broadly similar to other DMTs except that there is a          #
# particular risk of PML.                                                      #
# ============================================================================ #


natalizumabProcessing <- function(population, currentCycleEDSSCol, 
                                  nextCycleDMTColname,
                                  cycleNum){
  
    # Measure population to check matching incoming/outgoing

      popSize <- nrow(population)


    # Merge in PML risk by anti-JCV seropositivity and time on
    # natalizumab for subset

        population <-
         population[PMLRisk, on =.(timeOnCurrentDMT = natalizumabDuration,
                       antiJCV = seropositivity),
        PMLRisk := i.PMLRisk]


    # Test if less than random number and assign PML accordingly, and also
    # mark generically that intolerance has occurred

        population[PMLDevelopmentOutcomeRandNo - PMLRisk < 1e-10,
                                `:=` (DMTIntolerance = 1L,
                                      PML = 1L)]


    # If PML has occurred increment EDSS by 2 (capped at 9)

        population[PML == 1, c(currentCycleEDSSCol) :=
                     get(currentCycleEDSSCol) + 2]

        population[PML == 1 & get(currentCycleEDSSCol) > 9,
                   c(currentCycleEDSSCol) := 9L]


    # Tidy

        population[, `:=` (PMLRisk = NULL)]



    # Assign mortality to those experiencing PML and mark relevant
    # outcome fields

        population[PML == 1L &
                     (PMLDeathOutcomeRandNo - pmlMortalityRisk) < 1e-10,
                   PMLDeath := 1L]


        population[PMLDeath == 1L, `:=` (timeToDeath = cycleNum,
                                         ageAtDeath = currentAge,
                                         traceComplete = 1L,
                                         RRMSCohort = 0L,
                                         SPMSCohort = 0L)]


    # Split into those who have died from PML, survivors, and no PML

        natalizumab_PMLDeaths <- population[PMLDeath == 1L]
        
        natalizumab_PMLSurvivors <- population[is.na(PMLDeath) & PML == 1L]
        
        population <- population[is.na(PMLDeath) & is.na(PML)]


    # Split based on clinical activity

        natalizumabSwitch_clinicalActivity <- population[clinicalActivity > 0]

        population <- population[clinicalActivity == 0]


    # In those that are responsive test for intolerance

        population <- determineIntolerance(population)

        natalizumabSwitch_intolerance <- population[DMTIntolerance == 1]

        natalizumab_continuing <- population[is.na(DMTIntolerance)]
        natalizumab_continuing[, c(nextCycleDMTColname) :=
                                 currentCycleDMT]
    

    # Create list to return

        returnList <- list(natalizumab_PMLDeaths = natalizumab_PMLDeaths,
                           natalizumabSwitch_clinicalActivity = natalizumabSwitch_clinicalActivity,
                           natalizumabSwitch_intolerance = rbind(natalizumabSwitch_intolerance,
                                                                 natalizumab_PMLSurvivors),
                           natalizumab_continuing = natalizumab_continuing)


    # Check incoming and outgoing population sizes

        checkSize <- sum(sapply(returnList, nrow))

    if (abs(popSize - checkSize) >= 1){

      stop('!!! Incoming and outgoing natalizumab population size mismatch. Aborted. !!!')
    }


    # Return

    return(returnList)

}