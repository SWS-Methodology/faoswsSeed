##' Function for obtaining the area harvested/sown data
##' 
##' This function pulls the trade data from the database.  The main function
##' pulling the data is faosws::GetData, but additional steps are performed by
##' this function (such as setting up the appropriate pivot, adding variables
##' which are missing from the data as NA's, and setting data with missing
##' flags and 0 values to NA values).
##' 
##' @param dataContext The context for the data, as generated by the SWS.  This
##' object can be created via a call like swsContext.datasets[[1]] (assuming
##' the user is running this script on the SWS or after a call to
##' GetTestEnvironment).
##' @param areaSownElementCode The element code providing the dimension which
##' corresponds to the area sown variable in the database.
##' @param areaHarvestedElementCode The element code providing the dimension
##' which corresponds to the area harvested variable in the database.
##' @param seedElementCode The element code providing the dimension which
##' corresponds to the seed variable in the database.
##'  
##' @return A data.table object containing the data queried from the database.
##' 
##' @export
##'  

getAreaData = function(dataContext, areaSownElementCode = "5025",
                       areaHarvestedElementCode = "5312",
                       seedElementCode = "5525"){
    
    ## Data Quality Checks
    stopifnot(is(dataContext, "DatasetKey"))
    stopifnot(is(areaSownElementCode, "character"))
    stopifnot(is(areaHarvestedElementCode, "character"))
    stopifnot(is(seedElementCode, "character"))
    
    ## Set some name variables needed to pull this data
    areaVar = "geographicAreaM49"
    yearVar = "timePointYears"
    itemVar = "measuredItemCPC"
    elementVar = "measuredElement"
    valuePrefix = "Value_measuredElement_"
    flagObsPrefix = "flagObservationStatus_measuredElement_"
    flagMethodPrefix = "flagMethod_measuredElement_"
    
    ## Setups
    requiredElements = c(areaSownElementCode, areaHarvestedElementCode,
                         seedElementCode)
    
    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )
    
    ## Check the code for area
    slot(slot(dataContext, "dimensions")$measuredElement, "keys") =
        requiredElements
    
    ## Query the data
    query = GetData(
        key = dataContext,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    
    ## Find and fill in missing elements
    missingElements =
        requiredElements[which(!paste0(valuePrefix, requiredElements) %in%
                                   colnames(query))]
    lapply(missingElements,
           FUN = function(x){
               query[, `:=`(paste0(c(valuePrefix, flagObsPrefix,
                                     flagMethodPrefix), x),
                            list(as.numeric(NA), as.character(NA),
                                 as.character(NA)))]
           }
    )
    
    ## Make zero (M) as NA (M)
    lapply(requiredElements,
           FUN = function(x){
               remove0M(data = query,
                        value = paste0(valuePrefix, x),
                        flag = paste0(flagObsPrefix, x))
           })
    query[, timePointYears := as.numeric(timePointYears)]
    setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    ## list(query = query,
    ##      prefixTuples = prefixTuples)
    
    cleanData = function(x, class){
        x = sapply(x, function(y) ifelse(is.null(y), NA, y))
        if(is(x, "list"))
            x = do.call("c", x) # coerce list type to vector
        x = as(x, class)
        return(x)
    }
    
    ## Adjust data types to avoid later errors
    for(element in requiredElements){
        warning("Current code fixes issue SWS-797 manually, update when resolved")
        valueVariable = paste0(valuePrefix, element)
        query[, c(valueVariable) := cleanData(get(valueVariable), "numeric")]
        methodVariable = paste0(flagMethodPrefix, element)
        query[, c(methodVariable) :=
                  cleanData(get(methodVariable), "character")]
        obsVariable = paste0(flagObsPrefix, element)
        query[, c(obsVariable) := cleanData(get(obsVariable), "character")]
    }

    ### Some values don't exist in the database, and thus are given NA's.
    ### However, for our purposes, these should be 0M values.
    # area sown
    areaSownVariables = paste0(c("flagObservationStatus",
                                 "flagMethod"), "_measuredElement_",
                               areaSownElementCode)
    query[is.na(get(paste0("Value_measuredElement_", areaSownElementCode))),
          (areaSownVariables) := list("M", "u")]
    # area harvested
    areaHarvestedVariables = paste0(c("flagObservationStatus",
                                      "flagMethod"), "_measuredElement_",
                                    areaHarvestedElementCode)
    query[is.na(get(paste0("Value_measuredElement_", areaHarvestedElementCode))),
          (areaHarvestedVariables) := list("M", "u")]
    # seed
    seedVariables = paste0(c("flagObservationStatus",
                             "flagMethod"), "_measuredElement_",
                           seedElementCode)
    query[is.na(get(paste0("Value_measuredElement_", seedElementCode))),
          (seedVariables) := list("M", "u")]
    
    return(query)
}