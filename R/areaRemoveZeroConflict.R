##' Remove Zero Conflicts
##'
##' The function examines two variables of a data.table object and helps 
##' to evaluate their feasible combinations. In particular if variable1 is
##' supposed to be greater then variable2 - such as areaSown and areaHarvested-
##' all the items characterized by variable1=0 and variable2>0 are marked as
##' missingObservationFlag_variable1 and missingMethodFlag_variable1
##'
##' @param data The data table object.
##' @param value1 The column name of data corresponding to the first variable.
##' @param value2 The column name of data corresponding to the second variable.
##' @param observationFlag1 The column name of data containing the observation
##' flag for the first variable.
##' @param observationFlag2 The column name of data containing the observation
##' flag for the second variable.
##' @param methodFlag1 The column name of data containing the method flag for
##' the first variable.
##' @param methodFlag2 The column name of data containing the method flag for
##' the second variable.
##' @param missingObservationFlag The flag (character value) which should be
##' placed in the observation flag columns to signify a missing value.
##' @param missingMethodFlag The flag (character value) which should be placed
##' in the method flag columns to signify a missing value.
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified (some values are marked as missing if the have
##' conflicting zeroes).
##'
##' @export
##'

areaRemoveZeroConflict = function(data, value1, value2, observationFlag1,
                              methodFlag1, 
                              missingObservationFlag = "M",
                              missingMethodFlag = "u"){
  dataCopy = copy(data)
  
  ### Data Quality Checks
  stopifnot(is(dataCopy, "data.table"))
  cnames = c(value1, value2, observationFlag1, 
             methodFlag1)
  stopifnot(is(cnames, "character"))
  if(length(cnames) < 4){
    stop("One of the column names supplied (value1/2, observationFlag1 ",
         ", methodFlag1 is NULL!")
  }
  if(all(cnames %in% colnames(dataCopy))){
    ## Identify points where areaSown = 0 and areaHarvested != 0 
    filter1 = dataCopy[, get(value1) == 0 & get(value2) != 0]
   
    
    ## For problematic observations, set the zero value to missing.
    dataCopy[filter1 , `:=`(c(value1, observationFlag1, methodFlag1),
                            as.list(c(NA_real_, missingObservationFlag,
                                      missingMethodFlag)))]
  } else {
    warning("Selected columns are not present, no processing is performed")
  }
  dataCopy
}