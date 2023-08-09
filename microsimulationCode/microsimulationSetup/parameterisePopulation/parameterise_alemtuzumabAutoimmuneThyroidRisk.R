#==============================================================================#
#         Parameterise Alemtuzumab autoimmune thyroid disease risk             #
#===============================================================================
#                                                                              #
# Alemtuzumab carries a high risk of developing autoimmune thyroid disease     #
# predominantly Graves, and to a lesser extent Hashimoto's thyroiditis.        #
#                                                                              #
# Outcomes are based only on the occurrence of thyroid disease overall, and    #
# this is the only outcome to which PSA is applied.  If an autoimmune thyroid  #
# disease occurs this is assigned a subtype for interest only based on fixed   #
# proportions.                                                                 #
#                                                                              #
#==============================================================================#  


  # ----------------------------------------------------------------------------
  # Assign proportions of graves/hashimotos which aren't varying
  # ----------
  
        MSSample[, gravesRisk := 
                     alemtuzumabThyroidDisease[grepl('Graves', X1, 
                                                        ignore.case = TRUE),
                                               Mean]]
            
        MSSample[, hashimotosRisk := 
                       alemtuzumabThyroidDisease[grepl('Hashimoto', X1, 
                                                       ignore.case = TRUE),
                                                 Mean]]
        
  # ----------------------------------------------------------------------------


        
  # ----------------------------------------------------------------------------
  # Base case - assign mean proportion to all
  # ----------
    
      if (alemtuzumabAutoimmuneThyroidDisease_probabilisticSwitch == 0){
      
            MSSample[, autoimmuneThyroidRisk := 
                       alemtuzumabThyroidDisease[grepl('All thyroid disease', X1, 
                                                        ignore.case = TRUE),
                                                Events/N]]
      }


  # ----------------------------------------------------------------------------
  

  
  # ----------------------------------------------------------------------------
  # PSA - draw from beta distribution after setting seed and assigning at 
  # individual or parameter set level
  # ----------
    
      if(alemtuzumabAutoimmuneThyroidDisease_probabilisticSwitch == 1){
        
         MSSample <- cbind(MSSample, 
                           alemtuzumabThyroidDisease[grepl('All thyroid disease', X1, 
                                                      ignore.case = TRUE),
                                                    .(Events, N)]) 
      
        if (individualLevelPSA_probabilisticSwitch == 1){
          
          MSSample[, autoimmuneThyroidRisk := {dqset.seed(seedGroup, 
                                                          personID)
                                               
                                                dqrunif(seedRunN)
                                                
                                                rand = dqrunif(1)
                                            
                                                qbeta(rand, shape1 = Events,
                                                    shape2 = N - Events)
                                                },
                                            
                   by = .(seedGroup, personID)]
      
        }
         
      
         
        if (parameterSetLevelPSA_probabilisticSwitch == 1){
         
          
          MSSample[, autoimmuneThyroidRisk := {dqset.seed(parameterSetID, 
                                                          NULL)
                                               
                                               dqrunif(seedRunN)
                                                
                                               dqrunif(1)
                                               
                                               },
                   by = parameterSetID]
          
          MSSample[, autoimmuneThyroidRisk := qbeta(autoimmuneThyroidRisk, shape1 = Events,
                                                    shape2 = N - Events)]
          
        }
         
        
      # Tidy
      
         MSSample[, c('Events', 'N') := NULL]
  
      }
        

    # Increment seed run
      
         seedRunN <<- seedRun_thyroidDiseaseRisk

        
  # ----------------------------------------------------------------------------

