#==============================================================================#
#                     Annual Relapse Risk Coefficients                         #
#===============================================================================
#                                                                              #
# Relapse rates are taken from Tremlett et al. 2008, who regressed relapses    #
# on gender, age of onset, and disease duration, using two models with         #
# additive age of onset and disease duration and another interacting the two.  #
#                                                                              #
# The regression output with slightly modified coefficient names is included   #
# in inputValues.xlsx. The processing below extracts the coefficients and      #
# generates for each set of characteristics a row of coefficients, from which  #
# a mean ARR for that subset of the population is taken                        #
#                                                                              #
# In order to model relapses, mean and variance are needed.  Tremlett et al.,  #
# used a quasi-poisson regression model, which gives a dispersion parameter,   #
# which can be used to give rise to a variance conditional on the mean.        #
#                                                                              #
# This process therefore gives rise to a table with all combinations of the    #
# categorical characteristics included in the Tremlett regression, the mean    #
# ARR and a variance conditional on the mean.                                  #
#                                                                              #
# Derived from this are a gamma distribution shape and scale and a size        #
# parameter for use in a negative binomial distribution.  At present only a    #
# negative binomial or poisson are enabled but the gamma parameters are        #
# included to allow a pseudo-quasipoisson distribution to be constructed       #
# later if desired.                                                            #
#                                                                              #
# In PSA, values for each coefficient are drawn from a normal distribution     #
# based on the mean value and se for each coefficient; all are then summmed,   #
# including the constant and exponentiated.  This of course assumes            #
# independence of all variables.                                               #
#                                                                              #
#==============================================================================#
 
 
  
  # ----------------------------------------------------------------------------        
  # Create  expanded grid with each parameter set with each combination   
  # of onset age and disease duration since this varies with cycles, and 
  # include other regression variables
  # ----------
      
      annualRelapseRiskGrid <- 
        as.data.table(expand_grid(
          unique(MSSample[, .(parameterSetID, onsetAgeBand, femaleGender, sensoryOnset)]),
          diseaseDurationBand = diseaseDurationBands[, Group])
        )
  
      
  # ----------------------------------------------------------------------------    

      
      
  # ----------------------------------------------------------------------------    
  # Create a set of coefficient values either mean values for base case,
  # or at parameter set or individual level depending on PSA level
  # -----------
      
  
    # For base case simply merge mean values into relapse risk grid on 
    # relevant characteristics
   
      if (PSA_Switch == 0){
    
      
      annualRelapseRiskGrid[annualRelapseRiskInteractionCoefficients, 
                             onsetAgeDiseaseDurCoeff := i.Coefficient,
                            on = .(onsetAgeBand, diseaseDurationBand)]
      
      
      annualRelapseRiskGrid[annualRelapseRiskGenderCoefficients, 
                            femaleGenderCoeff := i.Coefficient,
                            on = .(femaleGender)]
      
      
      annualRelapseRiskGrid[annualRelapseRiskSensoryCoefficients, 
                            sensoryOnsetCoeff := i.Coefficient,
                            on = .(sensoryOnset)]
      
      
      annualRelapseRiskGrid <- cbind(annualRelapseRiskGrid,
                                    annualRelapseRiskInterceptCoefficients[, .(interceptCoeff = Coefficient)])
      
      }
  
    # ----
      
 
    # At parameter set level generate coefficients for each parameter set and 
    # merge in on relevant characteristics + parameter set ID
    
    if (parameterSetLevelPSA_probabilisticSwitch == 1) {
      
        # Identify coefficient tables
      
          coefficientsTablesList <- c('annualRelapseRiskGenderCoefficients',
                                       'annualRelapseRiskInteractionCoefficients',
                                       'annualRelapseRiskSensoryCoefficients',
                                      'annualRelapseRiskInterceptCoefficients')
      
          
        # Loop over tables, expanding by parameter set ID and draw from 
        # normal distribution after setting seed using parameter set ID
          
          coefficientsListTables <- 
        
            lapply(mget(coefficientsTablesList), function(DT) {

              table <- copy(DT)
              
              table <- as.data.table(
                      expand_grid(table, 
                                  unique(MSSample[, .(parameterSetID)]))
                      )
          
              table[, seedRunGroup := .GRP, by = Variable]
              table[, Coefficient := {
                    
                            dqset.seed(c(parameterSetID, NULL))
                    
                            dqrunif(seedRunN + seedRunGroup)
                            
                            rep(dqrnorm(1, 
                                     mean = as.numeric(Coefficient),
                                     sd = as.numeric(Std..Error)),
                                .N)
                            }, 
                    
                    by = .(Variable, parameterSetID)]
                
                
              table[is.na(Coefficient), Coefficient := 0L]
              
              table[, seedRunGroup := NULL]
          
          })
          
          
          names(coefficientsListTables) <- paste0(coefficientsTablesList, 'DT')
          
          
        # Unlist to global environment
          
           list2env(coefficientsListTables, envir = globalenv())
    
        
        # Merge all to annualRelapseRiskGrid on characteristics and parameter set
          
          annualRelapseRiskGrid[annualRelapseRiskInteractionCoefficientsDT, 
                             onsetAgeDiseaseDurCoeff := i.Coefficient,
                            on = .(onsetAgeBand, diseaseDurationBand, parameterSetID)]
      
      
          annualRelapseRiskGrid[annualRelapseRiskGenderCoefficientsDT, 
                            femaleGenderCoeff := i.Coefficient,
                            on = .(femaleGender, parameterSetID)]
      
      
          annualRelapseRiskGrid[annualRelapseRiskSensoryCoefficientsDT, 
                            sensoryOnsetCoeff := i.Coefficient,
                            on = .(sensoryOnset, parameterSetID)]
          
          
          annualRelapseRiskGrid <- cbind(annualRelapseRiskGrid,
                                    annualRelapseRiskInterceptCoefficientsDT[, .(interceptCoeff = Coefficient)])
                            
    
    
         
        } else if (individualLevelPSA_probabilisticSwitch == 1){
      
        
     # At individual level generate coefficients for each ID and 
     # merge in on combinedID
    
       # Identify coefficient tables
    
        coefficientsTablesList <- c('annualRelapseRiskGenderCoefficients',
                                       'annualRelapseRiskInteractionCoefficients',
                                       'annualRelapseRiskSensoryCoefficients',
                                      'annualRelapseRiskInterceptCoefficients')
    
        
        # Loop over tables, expanding by parameter set ID and draw from 
        # normal distribution after setting seed using parameter set ID
          
          coefficientsListTables <- 
        
            lapply(mget(coefficientsTablesList), function(DT) {
   
               table <- copy(DT)
               
               table <- as.data.table(
                  expand_grid(table, 
                              MSSample[, .(interventionID, combinedID, 
                                           seedGroup, personID)])
                  )
          
                        
                table[, seedRunGroup := .GRP, by = Variable]
                table[, Coefficient := { 
                                        dqset.seed(c(seedGroup, personID))
  
                                        dqrunif(seedRunN + seedRunGroup)
                                        
                                        rep(dqrnorm(1, 
                                                    mean = as.numeric(Coefficient[[1]]),
                                                    sd = as.numeric(Std..Error[[1]])),
                                            .N)
                                        },
                      
                       by = .(Variable, seedGroup, personID)]
                  
                  
                table[is.na(Coefficient), Coefficient := 0L]
                
                table[, seedRunGroup := NULL]
              

        })
          

        names(coefficientsListTables) <- paste0(coefficientsTablesList, 'DT')
  
          
        
      # Unlist to global environment
        
        list2env(coefficientsListTables, envir = globalenv())
  
      
      # Merge all to annualRelapseRiskGrid on characteristics and combinedID
        
         annualRelapseRiskGrid[annualRelapseRiskInteractionCoefficientsDT, 
                             onsetAgeDiseaseDurCoeff := i.Coefficient,
                            on = .(onsetAgeBand, diseaseDurationBand, combinedID)]
      
      
         annualRelapseRiskGrid[annualRelapseRiskGenderCoefficientsDT, 
                            femaleGenderCoeff := i.Coefficient,
                            on = .(femaleGender, combinedID)]
      
      
         annualRelapseRiskGrid[annualRelapseRiskSensoryCoefficientsDT, 
                            sensoryOnsetCoeff := i.Coefficient,
                            on = .(sensoryOnset, combinedID)]
         
         
         annualRelapseRiskGrid <- cbind(annualRelapseRiskGrid,
                                    annualRelapseRiskInterceptCoefficientsDT[, .(interceptCoeff = Coefficient)])
    
    }
      
  
  
          
  # ----------------------------------------------------------------------------    
      
    
      
  # ----------------------------------------------------------------------------    
  # Identify coefficients, sum and exponentiate to give relapse rate,
  # and add parameters for distributions
  # -----------
     
    # Identify coefficients
      
      ARRCoeffFields <- colnames(annualRelapseRiskGrid)[
        grepl( 'Coeff', colnames(annualRelapseRiskGrid))]
      
      
    # Sum coefficients and exponentiate to give estimate of ARR 
     
      annualRelapseRiskGrid[, estimate := exp(rowSums(.SD)),
                          .SDcols = grep('Coeff', 
                                         names(annualRelapseRiskGrid))]
    # Add scale parameter                                                       

      annualRelapseRiskGrid[, scale := ARRScaleParameter]
      
        
  # ----------------------------------------------------------------------------    

        
        
  # ----------------------------------------------------------------------------    
  # Tidy
  # ----
  
      # Increment seed
        seedRunN <<- seedRun_ARRCoefficients

       
  # ----------------------------------------------------------------------------                

        
#==============================================================================#