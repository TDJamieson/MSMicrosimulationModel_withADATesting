#==============================================================================#
#                                                                              #
#---                           Prepare inputs                               ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Inputs that require some pre-processing and/or addition of parameters in     #
# anticipation of PSA are prepared here.                                       #
#                                                                              #
# ============================================================================ #


  # ----------------------------------------------------------------------------
  # DMT lognormal parameters
  # ------
  
      DMTARREffects[, selog := deriveLogNormalParameters(IRR, 
                                                         CI_Lower,
                                                         CI_Higher)$selog]
                    
      DMTARREffects[, meanlog := deriveLogNormalParameters(mean = IRR, 
                                                           CI_Lower,
                                                           CI_Higher)$meanlog]

      
      DMTRRMSEDSSProgressionEffects[, selog := deriveLogNormalParameters(mean = RR, 
                                                                         CI_Lower,
                                                                         CI_Higher)$selog]
                    
      DMTRRMSEDSSProgressionEffects[, meanlog := deriveLogNormalParameters(mean = RR, 
                                                                           CI_Lower,
                                                                           CI_Higher)$meanlog]   

      
  # ----------------------------------------------------------------------------
  
      
      
  # ----------------------------------------------------------------------------
  # DMT intolerance risk beta parameters
  # ------
    
      DMTIntoleranceRisks[, c('shape1', 'shape2') := 
                            deriveBetaParameters(mu = rate_Intolerance, 
                                                 var = Var)]
  
  # ----------------------------------------------------------------------------
  
      
      
  # ----------------------------------------------------------------------------      
  # Transform mortality risk to rate for EDSS multipliers to be applied
  # ----
  
      mortalityRateTable <- 
        lifeTable[, .(age = age,
                       femGender = femGender,
                       mortalityRate = probabilityToRate(mortalityRisk))]
      
  # ----------------------------------------------------------------------------           

      
      
  # ----------------------------------------------------------------------------
  # Separate ARR regression table into coefficient sets and tidy 
  # ---------
  

    # Select interaction terms from regression table and split for 
    # identification

      annualRelapseRiskInteractionCoefficients <- 
        annualRelapseRisk[grepl("onsetAgeBand", Variable)]
      
      annualRelapseRiskInteractionCoefficients[, c('onsetAgeBand', 
                                                   'diseaseDurationBand') 
                                                    := tstrsplit(Variable, "\\*")]
      
      annualRelapseRiskInteractionCoefficients[, onsetAgeBand := as.integer(
                                                 str_extract(onsetAgeBand, '\\d')
                                                 )]
      
      annualRelapseRiskInteractionCoefficients[, diseaseDurationBand := as.integer(
                                                 str_extract(diseaseDurationBand, '\\d')
                                                  )]
    
      
    # Select gender row
      
      annualRelapseRiskGenderCoefficients <- 
        annualRelapseRisk[grepl("femaleGender", Variable)]
      
      annualRelapseRiskGenderCoefficients[, femaleGender := as.integer(
                                            str_extract(Variable, '\\d')
                                            )]
  
      
    # Select sensory onset row 
 
      annualRelapseRiskSensoryCoefficients <- 
          annualRelapseRisk[grepl("sensory", Variable)]
      
      annualRelapseRiskSensoryCoefficients[, sensoryOnset := as.integer(
                                            str_extract(Variable, '\\d')
                                            )]
      
    # Select intercept row
      
      annualRelapseRiskInterceptCoefficients <- 
          annualRelapseRisk[grepl("(Intercept)", Variable)]
      
      
  # ----------------------------------------------------------------------------
  
      

  # ----------------------------------------------------------------------------
  # Create list of stored swithcing tables for regneration in each cycle
  # ----
  
      switchingTablesStored <- copy(switchingTables)
        
  # ----------------------------------------------------------------------------