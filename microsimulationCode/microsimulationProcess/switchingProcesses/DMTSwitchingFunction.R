#==============================================================================#
#                     DMT switching - generic process                          #
#===============================================================================
#                                                                              #
# This function takes a population of individuals for whom a need for          #
# switching has been identified and assigns them a new DMT from an appropriate #
# table.  The table passed incorporates the switches which may be made *to*    #
# based on the disease type/level of activity the individual is in.            #
#                                                                              #
# This switching table contains 'combinedID' for all switching individuals and #
# remaining DMTs which they have not previously been exposed to and are        #
# included for their disease activity.                                         #
#                                                                              #
# There is a hierarchy of ifn-a - starting with the most potent but            #
# most likely to present intolerance 44mcg SC followed by 22mcg SC then        #
# 30mcg IM. This is incorporated by selecting all ifn-a in the merged switch   #
# table, ordering by ID, and keeping only the first record. NB for this to     #
# work the IDs must be ordered in that same hierarchy as above, and of course  #
# their IDs are built in here so must not be changed elsewhere.                #
#                                                                              #
# To make a switch, which is essentially random, the switching individuals     #
# are merged with the appropriate table containing their set of possible       #
# switches giving a table with repeated entries per person, one for each DMT   #
# to which they still can be switched. The IFN-a entries are then trimmed, as  #
# above, and the resulting table is randomly ordered and the first record for  #
# each individual is then taken to give a single switch to DMT for each person.#                                                                             
# 
# 
# ============================================================================ #


      switchFunction <- function(DT, switchDT, 
                                 switchingTableList = switchingTableList,
                                 cycle){
        
     # Merge the population with the appropriate switching table         -- 
        
        setorder(switchingTableList[[switchDT]], interventionID, parameterSetID, sequenceNo)
        switchingTable <- switchingTableList[[switchDT]][,.SD[1], 
                                                         by = .(interventionID, parameterSetID)]
        
        DT <- merge(DT,
                    switchingTable[, .(interventionID, parameterSetID, switchedTo = dmtID)],
                    by = c('interventionID', 'parameterSetID'), 
                    all.x = TRUE)
               
        
      # If no further DMTs are available in the switching group, 
      # then withdraw
        
        DT[is.na(switchedTo), switch := withdrawnID]
        
        DT[switch == withdrawnID, DMTsWithdrawn := 1L]
        
        DT[!is.na(switchedTo), switch := switchedTo]
        DT[, switchedTo := NULL]
        
        return(DT)
        
      }
      
      
  # ============================================================================
