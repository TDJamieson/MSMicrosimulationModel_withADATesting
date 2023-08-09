#==============================================================================#
#            Parameterise Alemtuzumab anti-drug antibody testing               #
#===============================================================================
#                                                                              #
#  This takes the parameters for determining Alemtuzumab ADA levels            #
#  and applies them to the whole sample.                                       #
#                                                                              #
# =============================================================================#


  # ----------------------------------------------------------------------------
  # Base case 
  # ----------
    
      if (PSA_Switch == 0){
      
            MSSample[, alemtuzumabADARisk := alemtuzumabADAProportion[, Mean]]
                     
      }
                       


  # ----------------------------------------------------------------------------
  

  
  # ----------------------------------------------------------------------------
  # PSA - !!! Needs Completing !!! 
  # ----------
    
      if(PSA_Switch == 1){
        
            MSSample[, alemtuzumabADARisk := alemtuzumabADAProportion[, Mean]]

      }
  
  # ----------------------------------------------------------------------------
