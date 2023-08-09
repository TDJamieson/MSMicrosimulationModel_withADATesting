#===============================================================================#
#===============================================================================#
#                                                                               #
#---            Generate random numbers for outcome determination            ---#
#                                                                               #
#===============================================================================#
#===============================================================================#  
  
#==============================================================================#
# The general approach to outcome determination is to generate a random        #
# uniform number and compare it to appropriate probability(ies) to determine   #
# which of a particular set of outcomes occurs.                                #
#                                                                              #
# In order to support reproducibility, the approach taken is to use a 64 bit   #
# random number generator provided by dqrng.  A two part seed is allowed.      #
# The approach taken is to use the 'seedGroup' as the first half of the seed   #
# and the personID as the second half.  Each individual is defined by their    #
# relevant characteristics - onset age, onset EDSS, gender etc. - and each     #
# character set which is given a unique identifier in the 'seedDT'.  This      #
# individual type ID which the seedGroup variable represents is therefore used #
# for the first part of seeding.  The number replicant that they are of that   #
# individual type given by personID is then the second half.                   #
#                                                                              #
# Using this seed a set of runif(0,1) are generated for each individual to     #
# cover as many cycles as needed for each outcome, which can be merged into    #
# each cycle.                                                                  #
#                                                                              #
# ============================================================================ #  


  # Define names of outcomes that random numbers are needed for and create a  --
  # table to include a random number to determine outcome for every cycle     --
  

      # Outcome names for every cycle outcomes
      
        outcomeNames <- list('mortality', 
                             'RRMSEDSSTransition',
                             'SPMSEDSSTransition',
                             'RRMStoSPMSTransition',
                             'ARR',
                             'DMTIntolerance')
      
      
      # Outcome names with cycle numbers
      
        outcomeRandomNumberFields <- unlist(
          lapply(outcomeNames, function(outcome) {
                                  
               paste0(outcome, 'OutcomeRandNo_Cycle', 1:nCycles)
        
            })
        )
      
      
      # Outcomes needing single value only
      
        singleOutcomeRandomNumberFields <- c('PMLDevelopmentOutcomeRandNo',
                                             'PMLDeathOutcomeRandNo',
                                             'AlemtuzumabAutoimmuneThyroidOutcomeRandNo',
                                             'autoimmuneThyroidTypeOutcomeRandNo', 
                                             'AlemtuzumabADAsOutcomeRandNo',
                                             'alemtuzumabFPOutcomeRandNo',
                                             'alemtuzumabFNOutcomeRandNo')
        
      # Combined 
      
        outcomeRandomNumberFields <- c(outcomeRandomNumberFields,
                                       singleOutcomeRandomNumberFields)
        
          
      
      # The number of these fields for which a runif needs to be generated
      
        noRandomsNeeded <- length(outcomeRandomNumberFields)
      
      
  # ----
  
      
  # Create DT from MSSample to hold random nos for each individual as       --
  # identified by their combined seedGroup and personID (i.e the same for   --
  # both intervention and comparator so they would have the same outcomes)  --

      outcomeRandNos <- MSSample[, .(combinedID, seedGroup, personID)]
      outcomeRandNos <- outcomeRandNos[, .SD[1], by = combinedID]

  # ----
  
      
  # Define a function to set seed and produce runifs for each individual      --
  # for the number of outcomes and cycles needed                              --
    
      produceNums <- function(seedGroup, personID){
        
        dqset.seed(c(seedGroup, personID))
        
        dqrunif(seedRunN) # Waste necessary number of runifs
        
        return(as.list(dqrunif(noRandomsNeeded)))
        
      }
      
  # ----
      
    
    
  # Assign using function defined above - this is a simply parallelisable     --
  # process, but sometimes it is more efficient on HPC to use a single node   --
  # so parallel RNG can be set with the parallelise variable                  --
  

  if(parallelise == 1){
    
      # Split roughly into equal parts matching number of cores
      outcomeRandNos[, parallelGroup := sample(1:nCores, .N, replace = TRUE)]
      outcomeRandNos <- split(outcomeRandNos, by = 'parallelGroup')
         
      # Set up clusters
      
         cl <- makeCluster(nCores)
      
         clusterExport(cl, c('outcomeRandNos', 'produceNums', 'seedRunN',
                             'outcomeRandomNumberFields', 'noRandomsNeeded'))
      
      
         clusterEvalQ(cl,{library(data.table)
                          library(dqrng)})      
  
         
      # Run along list that has been split and assign outcome random nos
         
         outcomeRandNos <- 
           parLapply(cl, outcomeRandNos, function(DT, fields) {
         
                     DT[, c(fields) := produceNums(seedGroup, personID), 
                     by = .(seedGroup, personID)]
              
                     }, fields = outcomeRandomNumberFields)
      
         
      # Stop cluster and recreate outcome nos DT from split list

         stopCluster(cl)

         outcomeRandNos <- rbindlist(outcomeRandNos)
         
             
      } else {
        
        
        outcomeRandNos[, c(outcomeRandomNumberFields) := produceNums(seedGroup, personID), 
                         by = .(seedGroup, personID)]
        
      }
         
  
      # Tidy
  
         remove(list = names(outcomeRandomNumberFields),
                              noRandomsNeeded, produceNums)#
         
         seedRunN <<- seedRun_outcomeRands
         
  
         
  # ============================================================================


