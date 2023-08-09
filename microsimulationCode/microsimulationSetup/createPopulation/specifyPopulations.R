#==============================================================================#
#==============================================================================#
#                                                                              #
#---            Create cohorts to run through microsimulation               ---#
#                                                                              #
#==============================================================================#
#==============================================================================#
  
#==============================================================================#
#                                                                              #
# To create a deterministic population, an onset age range, age to start       #
# in the model range, EDSS at onset, and gender are taken from the population  #
# grid in the input spreadsheet.  A desired number of replicants for each      #
# characteristic set is also taken, either specified in the row in the grid    #
# or, where the equal sample size option is set, from there.                   #
#                                                                              #
# For each row in the population a grid expand is then done so that there is   #
# an entry for each range of age of onset, model start age, EDSS at onset,     #
# and both genders if that is specified.                                       #
#                                                                              #
# This creates a grid with a single row for every characteristic set, i.e      #
# 'type' of individual to be run through the model, with an accompanying       #
# number of replicants desired.  In addition to this parameter sets can be     #
# added, with an ID for each parameter set used for seeding.  This results in  #
# a grid with each characteristic set replicated for each parameter set.       #
# This total population is then split into chunks of the size set as           #
# maxPopSize so that each individual profile for each parameter set is run     #
# in several chunks of the right size  - with starting personID and ending     #
# personID specified. So e.g. person 1 with parameter set 1 might be           #
# replicated in three rows with personID 0-100,000, 100,000-200,000 and        #
# 200,000- 250,000                                                            #
#------------------------------------------------------------------------------#


  # ----------------------------------------------------------------------------
  # The code sourced below will take the population grid specified in the 
  # input spreadsheet and create a tidied row for each characteristic set 
  # desired 
  # -----

        source(paste0(microsimulationSetupDirectory, 'createPopulation/',
                      'population_createSpecified.R'))

  # ----------------------------------------------------------------------------
 

  # ----------------------------------------------------------------------------
  # The grid created above now needs to be split into population sizes that
  # are feasible for computation - if the population running through the model
  # is too big it will exceed RAM and the process won't be able to complete.
  # The maximum size that can run through the model is specified in the 
  # model controls spreadsheet
  # -----
 
    # Merge to seedDT give seed group for each set of profile
    # characteristics

        MSSample[seedDT, seedGroup := i.seedGroup,
                  on = .(onsetAge, currentAge, femaleGender, onsetEDSS)]
  
  
    # Split into individual populations on seedGroup
    
        MSSampleSplits <- split(MSSample, by = 'seedGroup')
        
    
    # Now expand into the number of runs that will be needed given the maximum 
    # population size that can run through the model and specify start and end
    # person ID
    
        MSSampleSplits <- lapply(MSSampleSplits, function(row) {

            if(row[, noReplicants] <= maxPopSize){
  
                row[, `:=` (startID = startingPersonID,
                            endID = row[, noReplicants])]
              

              } else {
          
            noPops = row[, noReplicants]/maxPopSize
            noReplicants = row[, noReplicants]
  
            startID <- c(0:round(noPops)*maxPopSize+startingPersonID)
            endID <-   c(1:round(noPops)*maxPopSize+startingPersonID, noReplicants+startingPersonID)

            row <- cbind(row, startID, endID)
            row[, endID := endID - 1]

            as.data.table(row)
            
            }
  
        })
      
        
    # Re-bind table and now split into tables up to where the max popsize is exceed
    
      MSSampleSplits <- do.call(rbind, MSSampleSplits)
      
      MSSampleSplits <- MSSampleSplits[!(startID >= endID)]
      
    
    # Add number of individuals in each row
    
      MSSampleSplits[, popSize := endID-startID+1]  
  
      
    # Create list to hold splits and initialise element counter
    
      tempList <- vector(mode = 'list')
      
      element = 1

            
   # Duplicate for all parameter set IDs defined in parameter set seed range and create splits
      
      MSSampleSplits <- as.data.table(expand_grid(MSSampleSplits,
                                                  parameterSetID = PSASeedRange))
      while (nrow(MSSampleSplits) > 0){
 
        MSSampleSplits[, runningTotalPop := cumsum(popSize)]
        rowNumTo <- MSSampleSplits[runningTotalPop <= maxPopSize, .I[.N]]
      
           
        tempList[[element]] <- MSSampleSplits[1:rowNumTo]
        
        MSSampleSplits <<- MSSampleSplits[!1:rowNumTo]
        
        element <<- element + 1
        
      }
    

      MSSampleSplits <- tempList
      remove(MSSample, tempList, element)
        
  # ----------------------------------------------------------------------------    
  
     
      
 

          