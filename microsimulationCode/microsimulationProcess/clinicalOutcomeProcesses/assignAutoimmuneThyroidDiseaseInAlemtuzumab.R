#==============================================================================#
#                                                                              #
#--- Assign autoimmune thyroid disease in those two years post-alemtuzumab  ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Autoimmune thyroid disease occurs in about a third of people mean 2.5 years  #
# post dose 2, with 60% being Graves, 15% being Hashimoto's and the rest other #
# niche and often transient disease.  The process first tests for presence or  #
# absence of any thyroid disease using the usual random number comparison.     #
# Then this is subdivided into Grave's and Hashimoto's on the basis of a       #
# second random number which essentially follows the cumulative sum approach   #
# used elsewhere but simplified since there are only two outcomes to be        #
# determined.                                                                  #
#                                                                              #
#==============================================================================#


  assignAlemtuzumabThyroidDisease <- function(population){

    population[AlemtuzumabAutoimmuneThyroidOutcomeRandNo - autoimmuneThyroidRisk < 1e-10,
                  `:=` (ATE = 1L,
                        yearsSinceATE = 1L)]
     
   population[(autoimmuneThyroidTypeOutcomeRandNo - gravesRisk < 1e-10) & ATE == 1, 
                 graves := 1L]
     
    population[(autoimmuneThyroidTypeOutcomeRandNo - (gravesRisk + hashimotosRisk) < 1e-10) 
               & is.na(graves) & ATE == 1, hashimotos := 1L]

     
   }