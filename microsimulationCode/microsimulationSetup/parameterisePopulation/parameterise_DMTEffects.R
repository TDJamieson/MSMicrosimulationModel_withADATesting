#==============================================================================#
#                                DMT effects                                   #
#===============================================================================
#                                                                              #
# This creates two data tables of effects for DMTs on ARR and on RRMS EDSS     #
# progression. The capability to vary DMT effect by cycle is built in both     #
# here and in the microsimulation process.  If DMT effects are varied by cycle #
# this is currently assigned as a random effect for each cycle parameterised   #
# based on the DMT mean and se - i.e. there is not a consistent correlation    #
# between an individual and DMT effect, and a value generated for each DMT     #
# for each cycle.                                                              #
#                                                                              #
# If a single DMT effect size for each person or parameter set is all that is  #
# required then a single value is assigned and in the microsimulation process  #
# this is reassigned in each cycle by updating the name of the field.          #
#                                                                              #
# If probabilistic modelling is off then every individual will take the mean   #
# for the number of cycles specified - a single cycle, or all cycles.          #
#                                                                              #
# If DMT effects are switched off then every DMT gets assigned RR/IRR of 1.    #
#                                                                              #
# If probabilistic modelling is switched on then effects are drawn from a      #
# lognormal distribution. This may be done at individual level or parameter    #
# set level - i.e. at individual level all get a random draw after setting     #
# personal seeds based on seed group and person ID; at parameter set level     #
# the seed is set based on the parameter set ID so there will be an entire     #
# population for each parameter set ID with a single random draw.              #
#                                                                              #
#==============================================================================# 
  
 
  # ----------------------------------------------------------------------------
  # Initialise data tables to hold DMT effects
  # ------------

      # Define cycle fields to be added - this can be either a DMT effect for
      # each cycle, or a single DMT effect to be imported for each cycle
      # depending on the DMT a individual is on
      

        if(varyDMTEffectByCycle_Switch == 1){
    
          DMTARREffectFields <- paste0('DMTARREffect_Cycle', 1:nCycles)
          DMTRRMSEDSSProgressionEffectFields <- paste0('DMTRRMSEffect_Cycle', 
                                                       1:nCycles)

        }


        if(varyDMTEffectByCycle_Switch == 0){
    
          DMTARREffectFields <- paste0('DMTARREffect_Cycle0')
          DMTRRMSEDSSProgressionEffectFields <- paste0('DMTRRMSEffect_Cycle0')

        }


          
      # Create data tables containing all DMTs for all PSA IDs and all individuals

        DMTARREffectsDT <- as.data.table(
                          expand_grid(unique(MSSample[, .(fullID, parameterSetID, interventionID)]), 
                                      DMTARREffects)
                          )
    
        
        DMTRRMSEDSSProgressionEffectsDT <- 
          as.data.table(
                         expand_grid(unique(MSSample[, .(fullID, parameterSetID, interventionID)]),  
                                      DMTRRMSEDSSProgressionEffects)
          )
          
        
      # Assign NA to initialise fields holding values for cycles
        
        DMTARREffectsDT[, c(unlist(DMTARREffectFields)) := NA_real_] 
        
        DMTRRMSEDSSProgressionEffectsDT[, 
                        c(unlist(DMTRRMSEDSSProgressionEffectFields)) := NA_real_]
        
        
  # ----------------------------------------------------------------------------      
  
        
        
  # ----------------------------------------------------------------------------
  # If DMT effects are off then set RR/IRR to 1 for all individuals for all 
  # DMTs for all cycles
  # ----------
        
      if(DMTEffect_Switch == 0){
        
   
      # Assign values
        
          DMTARREffectsDT[, c(unlist(DMTARREffectFields)) := 1] 
          
          DMTRRMSEDSSProgressionEffectsDT[, 
                        c(unlist(DMTRRMSEDSSProgressionEffectFields)) := 1]
          

      # Tidy 
            
          DMTARREffectsDT[, c('seedGroup', 'personID', 'IRR') := NULL]
          
          DMTRRMSEDSSProgressionEffectsDT[, c('seedGroup', 'personID',
                                            'RR') := NULL]
        
      }
  
  # ----------------------------------------------------------------------------            
   
        
        
  # ----------------------------------------------------------------------------                 
  # If PSA is switched off but DMT effects are on, ie. base case, assign mean 
  # treatment effect for all DMTs for all individuals for all cycles
  # ---------
      
      if(DMTEffect_Switch == 1 & 
         DMTTreatmentEffects_probabilisticSwitch == 0){
         
        # Assign values
        
          DMTARREffectsDT[, c(unlist(DMTARREffectFields)) := IRR] 
          
          DMTRRMSEDSSProgressionEffectsDT[, 
                        c(unlist(DMTRRMSEDSSProgressionEffectFields)) := RR]
          
      }    
        
  
  # ----------------------------------------------------------------------------  
        
        
        
  # ----------------------------------------------------------------------------                 
  # If DMT effects are on and PSA is switched on then draw values at 
  # appropriate level
  # ---------      
        
    if (DMTEffect_Switch == 1 & 
        DMTTreatmentEffects_probabilisticSwitch == 1){
      
     
  # ----------------------------------------------------------------------------                 
  # If PSA is at individual level then draw a DMT effect for each individual  --
  # and assign it to each cycle for that individual                           --
           
    if (individualLevelPSA_probabilisticSwitch == 1){
     
          # Assign DMT effect for each individual, which is identified by
          # their seed group and personID
           
              DMTARREffectsDT <- as.data.table(
                expand_grid(DMTARREffectsDT,
                            MSSample[, .(seedGroup, personID, interventionID)])
              )
              
              DMTARREffectsDT[, seedRunGroup := .GRP, by = dmtID]
              
              DMTARREffectsDT[dmtID != 0 & dmtID != withdrawnID & dmtID != 100,
                          c(unlist(DMTARREffectFields)) :=
                            produceLogNormalValues(seed1 = seedGroup, 
                                                   seed2 = personID,
                                                   seedRunGroup = seedRunGroup,
                                                   meanlog = meanlog, 
                                                   selog = selog),
                            by = .(seedGroup, personID, dmtID)]
      
              seedRunN <<- seedRun_DMTARREffects
       
              DMTARREffectsDT[, seedRunGroup := NULL]

              
              
              DMTRRMSEDSSProgressionEffectsDT <- as.data.table(
                expand_grid(DMTRRMSEDSSProgressionEffectsDT,
                            MSSample[, .(seedGroup, personID, interventionID)])
              )
        
              DMTRRMSEDSSProgressionEffectsDT[, seedRunGroup := .GRP, by = dmtID]
              
              DMTRRMSEDSSProgressionEffectsDT[dmtID != 0 & dmtID != withdrawnID & dmtID != 100,
                          c(unlist(DMTRRMSEDSSProgressionEffectFields)) :=
                            produceLogNormalValues(seed1 = seedGroup, 
                                                   seed2 = personID,
                                                   seedRunGroup = seedRunGroup,
                                                   meanlog = meanlog, 
                                                   selog = selog),
                                      by = .(seedGroup, personID, dmtID)]
              
              seedRunN <<- seedRun_DMTRRMSEDSSProgressionEffects
              
              DMTRRMSEDSSProgressionEffectsDT[, seedRunGroup := NULL]

      
              
      }
           
           
     
           
  # If PSA is at parameter set level then draw a DMT effect for each          --
  # parameter set and assign it to all individuals assigned with that set     --
  
    if (parameterSetLevelPSA_probabilisticSwitch == 1){
   
        # Assign DMT effect for each parameter set, identified by parameterSetID
            
            DMTARREffectsDT[, seedRunGroup := .GRP, by = dmtID]
      
            DMTARREffectsDT[dmtID != 0 & dmtID != withdrawnID & dmtID != 100,
                        c(unlist(DMTARREffectFields)) :=
                          produceLogNormalValues(seed1 = parameterSetID, 
                                                 seed2 = dmtID, 
                                                 seedRunGroup = 0,
                                                 meanlog = meanlog, 
                                                 selog = selog),
                          by = .(parameterSetID, dmtID)]
      
           seedRunN <<- seedRun_DMTARREffects
     
           DMTARREffectsDT[, seedRunGroup := NULL]
            
           
            
           DMTRRMSEDSSProgressionEffectsDT[, seedRunGroup := .GRP, by = dmtID]
          
           DMTRRMSEDSSProgressionEffectsDT[dmtID != 0 & dmtID != withdrawnID & dmtID != 100,
                        c(unlist(DMTRRMSEDSSProgressionEffectFields)) :=
                          produceLogNormalValues(seed1 = parameterSetID, 
                                                 seed2 = dmtID, 
                                                 seedRunGroup = 0,
                                                 meanlog = meanlog, 
                                                 selog = selog),
                                    by = .(parameterSetID, dmtID)]
            
           seedRunN <<- seedRun_DMTRRMSEDSSProgressionEffects

           DMTRRMSEDSSProgressionEffectsDT[, seedRunGroup := NULL]
           
          
    }
        
      
    }
        
  # ----------------------------------------------------------------------------                 
        
        
        
  # For placebo and withdrawn IDs assign relative risks of 1                  --
        
      DMTARREffectsDT[dmtID == 0 | dmtID == withdrawnID, 
                    c(unlist(DMTARREffectFields)) := 1]
    
      DMTRRMSEDSSProgressionEffectsDT[dmtID == 0 | dmtID == withdrawnID, 
                    c(unlist(DMTRRMSEDSSProgressionEffectFields)) := 1]
        
        
        
  # Assign the same alemtuzumab effect for all doses and 'complete'           --
      
      DMTARREffectsDT <- 
        rbind(DMTARREffectsDT[!dmtID == alemtuzumabCompleteID & 
                              !dmtID == alemtuzumabCourse1CompleteID & 
                              !dmtID == alemtuzumabDose2ID & 
                              !dmtID == alemtuzumabDose3ID],
              DMTARREffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabCompleteID,
                                                              Name = 'Alemtuzumab complete')],
              DMTARREffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabCourse1CompleteID,
                                                              Name = 'Alemtuzumab course 1 complete')],
              DMTARREffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabDose2ID,
                                                              Name = 'Alemtuzumab dose 2')],
              DMTARREffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabDose3ID,
                                                              Name = 'Alemtuzumab dose 3')])
      
      
       DMTRRMSEDSSProgressionEffectsDT <- 
        rbind(DMTRRMSEDSSProgressionEffectsDT[!dmtID == alemtuzumabCompleteID & 
                                              !dmtID == alemtuzumabCourse1CompleteID & 
                                              !dmtID == alemtuzumabDose2ID & 
                                              !dmtID == alemtuzumabDose3ID],
              DMTRRMSEDSSProgressionEffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabCompleteID,
                                                                             Name = 'Alemtuzumab complete')],
              DMTRRMSEDSSProgressionEffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabCourse1CompleteID,
                                                                              Name = 'Alemtuzumab course 1 complete')],
              DMTRRMSEDSSProgressionEffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabDose2ID,
                                                                              Name = 'Alemtuzumab dose 2')],
              DMTRRMSEDSSProgressionEffectsDT[dmtID == alemtuzumabID][, `:=`(dmtID = alemtuzumabDose3ID,
                                                                             Name = 'Alemtuzumab dose 3')])
      
     
  # Assign the same cladribine effect for all doses and 'complete'            --
       
       DMTARREffectsDT <- 
        rbind(DMTARREffectsDT[!dmtID == cladribineCompleteID],
              DMTARREffectsDT[dmtID == cladribineID][, `:=`(dmtID = cladribineCompleteID,
                                                            Name = 'Cladribine complete')])
       
       
       DMTRRMSEDSSProgressionEffectsDT <- 
        rbind(DMTRRMSEDSSProgressionEffectsDT[!dmtID == cladribineCompleteID],
              DMTRRMSEDSSProgressionEffectsDT[dmtID == cladribineID][, `:=`(dmtID = cladribineCompleteID,
                                                                            Name = 'Cladribine complete')])
       
  # Tidy 
          
        DMTARREffectsDT[, c('IRR', 'selog', 'meanlog') := NULL]
        
        DMTRRMSEDSSProgressionEffectsDT[, c('RR', 'selog', 'meanlog') := NULL]
  
       
  # ============================================================================
