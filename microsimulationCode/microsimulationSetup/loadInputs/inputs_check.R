#==============================================================================#
#                              Check inputs                                    #
#===============================================================================
#                                                                              #
# Series of checks to make sure that inputs are reasonable and as expected     #                                                                        
#                                                                              #
#------------------------------------------------------------------------------#

  # Input population correctly filled ------------------------------------------

     
    # Do any population grid rows have missing values which are essential     --
        
          preSpecifiedPopulation[, naRow := 
                                 rowSums(is.na(.SD)), 
                               .SDcols = c('MinAge', 'MaxAge', 
                                           'MinEDSS', 'MaxEDSS', 
                                           'Gender')]
        
        if(nrow(preSpecifiedPopulation[naRow >0]) > 0){
          stop(paste0('  ***','\n\n',
                      '         Missing entries in spreadsheet input population ', 
                      'grid columns for at least one of Age, EDSS or Gender.',
                      '\n','         Process aborted ',
                      '\n\n','         ***')
          )
        }


    # Are DMT IDs missing without either random DMTs switched or a single     --
    # prespecified DMT given                                                  --

      preSpecifiedPopulation[,naRow := is.na(Starting.DMT.ID)]
      
        if(nrow(preSpecifiedPopulation[naRow >0]) > 0 
          & randomStartingDMT_Switch == 0
          & singleSpecifiedStartingDMT_Switch == 0) {
          
           stop(paste0('  ***','\n\n',
                        '         DMTs not all identified in population grid ',
                       'and neither random DMT nor single specified DMT dummy are set to 1',
                       '\n','         Process aborted ',
                        '\n\n','         ***')
            )
        
       }
      
      
    # Is no of replicants missing anywhere without having equal sample size    --
    # dummy set to 1                                                           --
      
        preSpecifiedPopulation[,naRow := is.na(No.Of.Replicants)]
        
        if(nrow(preSpecifiedPopulation[naRow >0]) > 0 
          & equalGroupSize_Switch == 0) {
          
           stop(paste0('  ***','\n\n', '         ',
                        'At least one row of population grid has no "no of replicants" ',
                       ' and the equal sample size dummy has not been set to 1,',
                        '\n','         Process aborted ',
                        '\n\n','         ***')
            )
        
        }
        
    # Are random DMTs and single DMT switches both on                         --
        
         if(randomStartingDMT_Switch == 1  
            & singleSpecifiedStartingDMT_Switch == 1) {
          
           stop(paste0('  ***','\n\n',
                        '         Both random starting DMTs and single specified ',
                        'starting DMT dummies have been switched to 1, which is impossible',
                        '\n','         Process aborted ',
                        '\n\n','         ***')
            )
        
        }
    
        
    # Are random DMTs or single DMT switches both on whilst the               --
    # grid has DMT values                                                     --
         preSpecifiedPopulation[, notNADMT := !is.na(Starting.DMT.ID)]
         
        if((randomStartingDMT_Switch == 1 
           | singleSpecifiedStartingDMT_Switch == 1)
           & nrow(preSpecifiedPopulation[notNADMT >0]) >0) {
  
         
          cat('\n','         ***','\n\n',
                        '         Warning: At least some starting DMT IDs',
                        'have been filled in the population grid but DMTs', '\n', '        ',
                        ' are also specified elsewhere either as all random, or',
                        'as a single pre-specified DMT, please ', '\n', '        ',
                        'stop process if this is unexpected',
                        '\n\n','         ***')
         Sys.sleep(10)
          
        }
         
         
    # Are replicant numbers filled whilst equal sample sizes are dummied on   --
    
         preSpecifiedPopulation[, notNAreplicants := !is.na(No.Of.Replicants)]
         
        if(equalGroupSize_Switch ==1 
            & nrow(preSpecifiedPopulation[notNAreplicants >0]) >0) {
  
         
          cat('\n','         ***','\n\n',
                        '         Warning: At least some starting No of Replicants values',
                        'have been filled in the population grid but equal', '\n', '        ',
                        'sample sizes have also been requested, please stop',
                        'the process if this is unexpected',
                        '\n\n','         ***')
         Sys.sleep(10)
          
        } 
         
      
    # Are included DMTs in RRMS EDSS Progression, and ARR effects tables?     --
      
      if(nrow(setdiff(includedDMTList[,.(dmtID, Name)],
                           DMTARREffects[,.(dmtID, Name)])) > 0 ){

           stop(paste0('  ***','\n\n',
                        '         There is a DMT in the included DMT list that is not ',
                        'in the relapse DMT effects table, cannot proceed.',
                        '\n','         Process aborted ',
                        '\n\n','         ***')
            )

      }
         
      if(nrow(setdiff(includedDMTList[,.(dmtID, Name)],
                           DMTRRMSEDSSProgressionEffects[,.(dmtID, Name)])) > 0 ){
          
           stop(paste0('  ***','\n\n',
                        '         There is a DMT in the included DMT list that is not ',
                        'in the RRMS EDSS Progression DMT effects table, ',
                        '\n','         cannot proceed. Process aborted ',
                        '\n\n','         ***')
            )
        
      }
         
      
         
    # Do DMTs have the same ID and name across all DMT tables?                --
    
      # All DMT Tables have been aggregated into 'allDMTTables' in 
      # loadInputs.R where some repetitive tidying was done 
      # Now take only dmtID and Name from each table, remove spaces,
      # and collapse into a single DT
      
         allDMTTables <- lapply(allDMTTables, function (table) {
           table <- table[, .(dmtID, Name)]
           table[, Name := gsub(" ","",Name)]
         })
         
         
         allDMTTables <- rbindlist(allDMTTables)
         
         
      # Count unique IDs and then unique combinations of ID and Name
      # This should be the same if all IDs are associated with the same drug
      # NB alemtuzumab and cladribine 'complete' need to be excluded or 
      # an error will be thrown
         
        
         uniqueIDandNames <- unique(allDMTTables[dmtID != alemtuzumabCompleteID & 
                                                 dmtID != alemtuzumabDose2ID & 
                                                 dmtID != cladribineCompleteID, 
                                                .(dmtID, Name)])
         
         duplicatedIDs <- uniqueIDandNames[duplicated(uniqueIDandNames[, .(dmtID)], fromLast = FALSE)|
                                          duplicated(uniqueIDandNames[, .(dmtID)], fromLast = TRUE)]                              
         
         
    
      # Test for equality 
      
         if (nrow(duplicatedIDs > 0)) {
          
           stop(paste0('\n\n','  ***','\n\n',
                        '         There are entries with the same DMT ID  ',
                        'with different names, microsimulation ',
                        '\n','         cannot proceed. Process aborted. The duplicates are stored in ',
                        '\n', ' the duplicatedIDs object.',
                        '\n\n','         ***')
            )
         }
      
         
         
       # Is more than one level of PSA switched on?                           --
        
         if(Reduce(`+`, mget(ls(pattern = 'PSA_Switch')))>1) {
           
           stop(paste0('\n\n','  ***','\n\n',
                        '         More than one level of PSA switched on  ',
                        '\n', '         cannot proceed. Process aborted. ',
                        '\n\n','         ***')
            )
         }


   # ----------------------------------------------------------------------------       
      
         
            
  # Tidy -----------------------------------------------------------------------
      
      preSpecifiedPopulation[, c('notNAreplicants', 'notNADMT', 'naRow')
                              := NULL]
      remove(allDMTTables, uniqueIDandNames, duplicatedIDs)
      
  # ----------------------------------------------------------------------------