#==============================================================================#
#                             Switching inputs                                 #
#===============================================================================
#                                                                              #
# This creates a number of data tables used in switching.  The first is a      #
# table that holds intolerance information with an intolerance risk for        #
# each DMT for individual.                                                     #
#                                                                              #
# There are also a number of DMT switching tables held in the input            #
# which direct choice of DMT when it is being switched.  These are expanded    #
# so that each individual has their own set of these tables to record whether  #
# or not they have already exhausted a particular DMT.                         #
#                                                                              #                                                                             
#------------------------------------------------------------------------------# 
          

 # DMT switching tables --------------------------------------------------------
 
 # A number of switching tables are used which direct choice of subsequent DMT.
 # In order to direct individual choice of DMT, previously exposed DMTs need 
 # to be removed from an individual's available switches.  This is done by 
 # creating an expanded DT for each switching table with each possible switch
 # and each individual's IDs. The intervention ID is used to differentiate
 # between the same person in the intervention and the comparator cohort.
 # The option to parallelise is included for individual level switching
 # since this is particularly burdensome.

      
      switchingTables <- lapply(switchingTablesStored, function(table) {
          
        # Identify no of DMTs so that a sequence of the right length
        # can be generated
        noOfDMTs <- nrow(table)
        
        # Expand so that each individual is represented
        DT <- copy(table)
        DT <- as.data.table(
              
                expand_grid(dmtID = table[, dmtID],
                  MSSample[, .(seedGroup, personID, 
                               interventionID, parameterSetID)])
                          )
       
   
        
    # To vary sequences for each individual (combinedID level) -----------------
        
        if (individualLevelDMTRandomness_Switch == 1){  
       
           
        # Order by intervention ID so that when sequences are repeated
        # they are repeated identically across intervention ID
        
          setorder(DT, interventionID)
          
          
        # If parallel processing set to operate then create and split 
        # across clusters
           
          if(parallelise == 1){
              
              DT[, parallelGroup := rep(sample(1:nCores, 1), .N), 
                 by = .(seedGroup, personID)]
              
              DT <- split(DT, by = 'parallelGroup' )
            
              
              cl <- makeCluster(nCores)
              clusterExport(cl, c('DT', 'seedRunN', 'noOfDMTs'), envir = environment())
              clusterEvalQ(cl, {library(data.table)
                                library(dqrng)}) 
              
              
              DT <- parLapply(cl, DT, function(table) {
                
                table[, sequenceNo := {
                            
                                        dqset.seed(seed = c(seedGroup, 
                                                            personID))
                                       
                                        dqrunif(seedRunN)
                                        
                                        rep(dqsample(1:noOfDMTs, noOfDMTs),
                                            2)
                                
                                       },
               
                     by = .(seedGroup, personID, parameterSetID)]
                
              })
            
             DT <- as.data.table(do.call(rbind, DT))
             
             stopCluster(cl)
          
             
        # If parallel processing not requested... 
           
          } else {
               
            DT[, sequenceNo := {
                
                                dqset.seed(seed = c(seedGroup, 
                                                    personID))
                               
                                dqrunif(seedRunN)
                                
                                rep(dqsample(1:noOfDMTs, noOfDMTs),
                                    2)
                                
                                },

              by = .(seedGroup, personID, parameterSetID)]
               
          }
        
           seedRunN <<- seedRunN + noOfDMTs
           
          
    # To vary sequences for each parameter set ---------------------------------
           
        } else if(parameterSetLevelSwitchSequencing_Switch == 1){
          
          setorder(DT, parameterSetID, interventionID)

          DT[, sequenceNo := rep({
              
              dqset.seed(seed = c(parameterSetID, 
                                  NULL))
            
              dqrunif(seedRunN)

              dqsample(1:noOfDMTs, noOfDMTs)}, .N/noOfDMTs),
              
              by = parameterSetID]
          
           seedRunN <<- seedRunN + noOfDMTs

           
          
    # To vary sequences for each characteristic set ----------------------------
          
        } else if(groupLevelSwitchSequencing_Switch == 1) {
           
          setorder(DT, parameterSetID, interventionID)
          
           DT[, sequenceNo := rep({

              dqset.seed(seed = c(seedGroup, 
                                  NULL))
             
              dqrunif(seedRunN)

              dqsample(1:noOfDMTs, noOfDMTs)},
              .N/noOfDMTs), 
              
              by = seedGroup]
           
            seedRunN <<- seedRunN + noOfDMTs

        }
        
      
        return(DT)
        
        })
      
      
      
      
 # -----------------------------------------------------------------------------     
