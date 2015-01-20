##' Get the area harvested/sown data
##' 
##' Imputation is currently quite simple: if the area sown variable has no
##' data, then area sown is assumed to be equal to area harvested.  If some
##' values exist and some are missing, than an average ratio is computed of
##' area sown to area harvested and this ratio is applied to all missing
##' values.
##' 
##' @param data The data.table object containing the seed data, usually as
##' created by getAreaData.
##' @param valueAreaSown The column name of data which contains the value of 
##' the area sown variable.
##' @param valueAreaHarvested The column name of data which contains the value of 
##' the area harvested variable.
##' @param flagObsAreaSown The column name of data which contains the
##' observation flag for the area sown variable.
##' @param flagObsAreaHarvested The column name of data which contains the
##' observation flag for the area harvested variable.
##' @param imputedObsFlag The value to be assigned to the observation status
##' flag for imputed observations.
##' @param imputedMethodFlag The value to be assigned to the method flag for
##' imputed observations.
##' 
##' @return No object is returned, instead the underlying data.table object is
##' modified.
##' 
##' @export
##' 

imputeAreaSown = function(data, valueAreaSown = "Value_measuredElement_5212",
          valueAreaHarvested = "Value_measuredElement_5312",
          flagObsAreaSown = "flagObservationStatus_measuredElement_5212",
          flagMethodAreaSown = "flagMethod_measuredElement_5212",
          flagObsAreaHarvested = "flagObservationStatus_measuredElement_5312",
          imputedObsFlag = "I", imputedMethodFlag = "e"){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    columnNames = c(valueAreaSown, valueAreaHarvested, flagObsAreaSown,
                    flagObsAreaHarvested)
    stopifnot(is(columnNames, "character"))
    stopifnot(columnNames %in% colnames(data))
    stopifnot(checkObservationFlag(imputedObsFlag))
    stopifnot(checkMethodFlag(imputedMethodFlag))
    
    if(all(is.na(data[[valueAreaSown]]))){
        data[, valueAreaSown := get(valueAreaHarvested), with = FALSE]
        data[, flagObsAreaSown := get(flagObsAreaHarvested), with = FALSE]
    } else {
        data[, ratio := mean(get(valueAreaSown)/get(valueAreaHarvested),
                             na.rm = TRUE)]
        data[, replaceIndex := is.na(get(valueAreaSown)) &
                 !is.na(get(valueAreaHarvested))]
        data[(replaceIndex), valueAreaSown := get(valueAreaHarvested) *
                 ratio, with = FALSE]
        data[(replaceIndex), flagObsAreaSown := imputedObsFlag,
             with = FALSE]
        data[(replaceIndex), flagMethodAreaSown := imputedMethodFlag,
             with = FALSE]
    }
    ## If the last operation was a data.table operation, the entire data.table
    ## may be returned.  Avoid that by assigning a dummy variable as the last
    ## operation.
    dummy = 0
}