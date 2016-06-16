##' Function to ensure that two value column does not contain conflicting zero
##' values.
##'
##' In the seed module for exaple, when area harvested is greater than zero, areaSown can not be
##' zero by definition (note that the opposite is allowed)
##'
##' @param data The hdata to be checked.
##' @param valueColumn1 The first variable for comparison.
##' @param valueColumn2 The second variable for comparison.
##' @param returnData logical, whether the data should be returned for pipe.
##' @param normalised logical, whether the data is normalised.
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##' @param getInvalidData logical, this will skip the test and extract the data
##'     that is invalid.
##' @return If getInvalidData is FALSE, then the data is returned when the test
##'     is cleared, otherwise an error. If getInvalidData is TRUE, then the
##'     subset of the data that is invalid is returned.
##'
##' @export
ensureNoConflictingAreaSownHarvested = function(data,
                                   valueColumn1,
                                   valueColumn2,
                                   returnData = TRUE,
                                   normalised = TRUE,
                                   denormalisedKey = "measuredElement",
                                   getInvalidData = FALSE){
  dataCopy = copy(data)
  if(normalised){
    dataCopy = denormalise(dataCopy, denormaliseKey = denormalisedKey)
  }
  
  ensureDataInput(dataCopy,
                  requiredColumn = c(areaSown, areaHarvested),
                  returnData = FALSE)
  conflicting =
   dataCopy[[areaSown]] < dataCopy[[areaHarvested]]
  
  zeroConflicting=
    dataCopy[[areaSown]]== 0 &
    dataCopy[[areaHarvested]]!=0
  
   
  
  conflictingArea =
    which(conflicting & zeroConflicting)
  
  invalidData = dataCopy[conflictingArea, ]
  
  if(getInvalidData){
   
    return(invalidData)
  } else {
    if(nrow(invalidData) > 0)
      stop("Conflict value exist between Area Sown and Area HArvested")
    

    
    message("No conflicting value exist")
    if(returnData)
      return(dataCopy)
  }
}