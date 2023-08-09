#==============================================================================#
#                       Parameterise mortality risk                            #
#===============================================================================
#                                                                              #
# Mortality is taken from UK ONS life tables which give age and                #
# gender-specific mortality risk.  A mortality multiplier is applied to        #
# this which is EDSS specific.  The source and derivation of this are          #
# presented in the excel spreadsheet.                                          #
#                                                                              #
# To produce EDSS-specific risks, the annual mortality risk, qx, for each      #
# age and gender is transformed to a rate, multiplied by EDSS-specific         #
# risk for each EDSS level and back-transformed to an annual risk.             #
#                                                                              #
#==============================================================================#  

  # Expand with all parameter set IDs
  
    mortalityEDSSMultiplierDT <- 
      as.data.table(expand_grid(EDSSMortalityMultipliers,
                                parameterSetID = PSASeedRange)
      )


  if(EDSSmortality_probabilisticSwitch == 1){

      if(parameterSetLevelPSA_probabilisticSwitch == 1){
         
        
        # Add lognormal parameters
        
         mortalityEDSSMultiplierDT[, selog := 
                                     deriveLogNormalParameters(Multiplier, 
                                                              CI_Lower,
                                                              CI_Higher)$selog]
                    
         mortalityEDSSMultiplierDT[, meanlog := 
                                     deriveLogNormalParameters(mean = Multiplier, 
                                                               CI_Lower,
                                                               CI_Higher)$meanlog]
         
         
         # Draw lognormal values
         
         mortalityEDSSMultiplierDT[, seedRunGroup := .GRP, by = EDSS]
    
         mortalityEDSSMultiplierDT[, Multiplier :=
                        produceLogNormalValues(seed1 = parameterSetID, 
                                               seed2 = EDSS, 
                                               seedRunGroup = 0,
                                               meanlog = meanlog, 
                                               selog = selog),
                        by = .(parameterSetID, EDSS)]
    
         seedRunN <<- seedRun_mortalityRisk
   

      }
    

  }
    

  
           
