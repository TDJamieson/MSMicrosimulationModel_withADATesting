#==============================================================================#
#                     Load project-specific inputs                             #
#===============================================================================
#                                                                              #
#  Using openxlsx this takes named ranges from excel and creates               #
#  data tables/matrices/values as desired, in the same way as the              #  
#  larger universal input loading process but from a project-specific          #
#  spreadsheet.                                                                #
# ============================================================================ #


# ------------------------------------------------------------------------------
# Load project-specific components
# ----------
    
    excelModel <- loadWorkbook(paste0(universalInputDirectory, 
                                      "alemtuzumabADAs.xlsx"))
    
    namedRegions <- openxlsx::getNamedRegions(excelModel)


    alemtuzumabADAProportion <- as.data.table(
      read.xlsx(excelModel, namedRegion = "alemtuzumabADAProportion",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    alemtuzumabTestingFPRate <- 
      as.numeric(
        read.xlsx(excelModel, namedRegion = "alemtuzumabTestingFPRate",
                  rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
      )
    
    alemtuzumabTestingFNRate <- 
      as.numeric(
        read.xlsx(excelModel, namedRegion = "alemtuzumabTestingFNRate",
                  rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
      )
    
    
    alemtuzumabEffectiveness <- as.numeric(
      read.xlsx(excelModel, namedRegion = "alemtuzumabEffectiveness",
                rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
    )


# ------------------------------------------------------------------------------