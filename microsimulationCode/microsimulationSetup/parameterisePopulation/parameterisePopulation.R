#==============================================================================#
#                             Assign parameters                                #
#===============================================================================
#                                                                              #
# This assigns all the parameters that are taken into the microsimulation      # 
# cycle.  Probabilistic vs mean values are determined inside each file         #
# dependent on the controls set in the input spreadsheet.                      #
#                                                                              #
#==============================================================================# 
  

  # DMT annualised relapse rate and EDSS progression effects

    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_DMTEffects.R'))

    print('Parameterising DMT Effects')


   # DMT intolerance

    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_DMTIntolerance.R'))
    
    print('Parameterising DMT Intolerance')
    


  # Mortality risk
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_mortalityRisk.R'))
    
    print('Parameterising Mortality Risk')
    


  # RRMS to SPMS transitions
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_RRMSSPMSTransitions.R'))
    
    print('Parameterising RRMS SPMS transitions')
    

 
  # RRMS EDSS transitions
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_RRMSEDSSTransitions.R'))
    
    print('Parameterising RRMS EDSS transitions')
    


  # SPMS EDSS transitions
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_SPMSEDSSTransitions.R'))
    
    print('Parameterising SPMS EDSS transitions')
    


  # Annual relapse rates
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_ARRCoefficients.R'))
    
    print('Parameterising relapse risk')
    


  # Alemtuzumab thyroid disease risk
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_alemtuzumabAutoimmuneThyroidRisk.R'))
    
    print('Parameterising ATE risk')
    


  # Alemtuzumab ADAs
  
    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/' , 
                  'parameterise_alemtuzumabADARisk.R'))
    
    print('Parameterising alemtuzumab ADAs')
    


# ==============================================================================