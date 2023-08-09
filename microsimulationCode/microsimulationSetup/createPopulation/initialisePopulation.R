#==============================================================================#
#==============================================================================#
#                                                                              #
#---                   Initialise microsimulation population                ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#  

#==============================================================================#
#                                                                              #
# This takes the population to be run through the microsimulation in this      #
# round of processing and does four things:                                    #
#                                                                              #
# 1. Expands the population so that the number of replicant individuals        #
#    desired for each characteristic set are created                           #
# 2. Assigns sensory vs non-sensory onset MS according to the proportion       #
#    in the input values spreadsheet                                           #
# 3. Duplicates the cohort if an intervention vs comparator group run has      #
#    been set in the spreadsheet                                               #
# 4. Assigns starting DMTs either as a single specified DMT from the           #
#    spreadsheet, or at individual, group or parameter set level               #
#                                                                              #
#==============================================================================#


  # ----------------------------------------------------------------------------
  # Assign sensory vs non-sensory onset symptoms and anti-JCV prevalence
  # according to the proportions specified in the input spreadsheet.  In order
  # to do this reproducibly a unique seed is created for each individual in
  # any characteristic set group in order to assign their sensory onset outcome
  # and anti-JCV outcome through the typical runif approach
  # ----------
 
      
      # Expand so that there are the number of replicants desired for
      # each row in the grid (if only person required, return single row)
      
          MSSample[, rowNum := .I]
          MSSample <- split(MSSample, by = 'rowNum')
          
          MSSample <- 
            lapply(MSSample, function(row) {
            
              if(row[, startID] != row[, endID]){
                  expand_grid(row,
                  personID = row[, startID]:row[, endID])
                
            } else {
              
              row[, personID := startID]
            }
            
          })
          
          MSSample <- as.data.table(do.call(rbind,MSSample))
                

      # Create runifs for sensory onset and anti-JCV presence, passing in
      # group and person ID

          MSSample[, c('sensoryRunif', 'JCVRunif') := {
            
                      dqset.seed(c(seedGroup, personID))
                      
                      dqrunif(seedRunN)
                      
                      as.list(dqrunif(2))
                      
                      },
                   
                   by = .(seedGroup, personID)]

          seedRunN <<- seedRun_sensoryJCV
          

      # Assign if runifs  are less than outcome value

          MSSample[sensoryRunif <= propSensoryOnset[, Proportion],
                   sensoryOnset := 1L]

          MSSample[sensoryRunif > propSensoryOnset[, Proportion],
                   sensoryOnset := 0L]
 

          MSSample[JCVRunif <= antiJCVPrevalence[, meanAsProp], antiJCV := 1L]

          MSSample[JCVRunif > antiJCVPrevalence[, meanAsProp], antiJCV := 0L]
          
      
      # Tidy 
      
          MSSample[, c('JCVRunif', 'sensoryRunif') := NULL]


  # ----------------------------------------------------------------------------



  # ---------------------------------------------------------------------------
  # If comparator group is switched on then replicate sample with both
  # ------

      if(comparatorCohort_Switch == 1){

        MSSample2 <- copy(MSSample)

        MSSample[, interventionCohort := 0L]
        MSSample2[, interventionCohort := 1L]


        MSSample <- rbind(MSSample, MSSample2)
        remove(MSSample2)

      }


      if(comparatorCohort_Switch == 0){

        MSSample[, interventionCohort := 1L]

      }

  # ----------------------------------------------------------------------------
  
          
          
  # ----------------------------------------------------------------------------
  # Create IDs - combined seed group and person IDs and also an interventionID
  # which marks whether an instance of a combinedID is intervention or 
  # comparator
  # ----------

          MSSample[, combinedID := paste0(seedGroup, "->", personID)]

          MSSample[, interventionID := paste0(combinedID, "_", interventionCohort)]
          
          MSSample[, fullID := paste0(interventionID, '.', parameterSetID)]

  # ----------------------------------------------------------------------------


          
  # ----------------------------------------------------------------------------    
  # If random DMTs are required, then randomly assign them using the list   
  # of DMTs in the appropriate treatment options table based on the treatment 
  # line     
  # ---------
  
      
      if (randomStartingDMT_Switch == 1){
      
        # Select appropriate DMT options table based on line table
        
          if(startingTreatmentLine == 1){
            
            DMTTable <- switchingTables$RRMSFirstLine
            
          } else if(startingTreatmentLine == 2) {
            
            DMTTable <- switchingTables$RRMSFirstLineClinicalFailure
            
          } else if(startingTreatmentLine == 3) {
            
            DMTTable <- switchingTables$RRMSSecondLineClinicalFailure
            
          }
        
          
      
        # Randomly sample from table identified - sampling level either at 
        # group or individual level by combining group and individual ID,
        # or just using individual ID
        
        
          if(individualLevelDMTRandomness_Switch == 1) {
          
            MSSample[, startingDMT := {dqset.seed(c(seedGroup, personID))
                                        
                                        dqrunif(seedRunN)
                                        
                                        rep(dqsample(size = 1, 
                                                     x = DMTTable[, dmtID],
                                                     replace = TRUE), 
                                                     .N)
                                        },
                     
                      by = .(seedGroup, personID)]
           
            
          } else if(groupLevelDMTRandomness_Switch) {
            
            MSSample[, startingDMT := {dqset.seed(c(seedGroup, NULL))
                                        
                                        dqrunif(seedRunN)
                                        
                                        rep(dqsample(size = 1, 
                                                     x = DMTTable[, dmtID],
                                                     replace = TRUE), 
                                                     .N)

                                        }, 
                     by = seedGroup]

            
          } else if(parameterSetLevelDMTRandomness_Switch == 1){
            
             MSSample[, startingDMT := {dqset.seed(c(parameterSetID, NULL))
                          
                                        dqrunif(seedRunN)
                                        
                                        rep(dqsample(size = 1, 
                                                      x = DMTTable[, dmtID],
                                                      replace = TRUE), 
                                                     .N)

                                        }, 
                      
                      by = parameterSetID]

            
          }
          
      
      # If single specified starting DMT then assign here
          
        } else if(singleSpecifiedStartingDMT_Switch == 1) {

                  
          MSSample[, startingDMT := singleSpecifiedStartingDMTID]
        
          
        }
          
      
      # Increment seedRunN 
      
        seedRunN <<- seedRun_DMT
  
                  
  # ----------------------------------------------------------------------------

  
        
  # ----------------------------------------------------------------------------
  
        noUniqueIndividuals <- nrow(unique(MSSample[, .(seedGroup, personID)]))
        noParameters <- length(PSASeedRange)
        
  # ----------------------------------------------------------------------------      

        
#==============================================================================#