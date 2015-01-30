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
##' @param imputationParameters A vector containing the parameters to use in
##' the imputation of areaSown variable.  If NULL, imputation is done by
##' computing the mean ratio of area harvested to area sown and applying that
##' to all cases.
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
          imputedObsFlag = "I", imputedMethodFlag = "e",
          imputationParameters = NULL){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    columnNames = c(valueAreaSown, valueAreaHarvested, flagObsAreaSown,
                    flagObsAreaHarvested)
    stopifnot(is(columnNames, "character"))
    stopifnot(columnNames %in% colnames(data))
    stopifnot(checkObservationFlag(imputedObsFlag))
    stopifnot(checkMethodFlag(imputedMethodFlag))
    stopifnot(imputationParameters$variable == "seed")

    ## If areaSown is available, compute ratios and use them to impute (either
    ## via ensembles or just with a pure mean).
    ## Also, only attempt to impute if >= 1 of the ratios is not NA
    seedRatios = data[, get(valueAreaSown) / get(valueAreaHarvested)]
    if(!all(is.na(seedRatios))){
        if(!is.null(imputationParameters)){ # Impute with ensemble
            ensureImputationInputs(data = data,
                                   imputationParameters = imputationParameters)
            data[, areaSownRatio := get(valueAreaSown)/get(valueAreaHarvested)]
            removeNoInfo(data = data, value = "areaSownRatio",
                         flag = flagObsAreaSown,
                         processingParameters = defaultProcessingParameters())
            remove0M(data = data, value = "areaSownRatio",
                     flag = flagObsAreaSown)
            imputeVariable(data = data,
                           imputationParameters = imputationParameters)
        } else { # Impute with simple mean
            data[, areaSownRatio := mean(get(valueAreaSown) / 
                                        get(valueAreaHarvested), na.rm = TRUE),
                 by = byKey]
            data[, replaceIndex := is.na(get(valueAreaSown)) &
                     !is.na(get(valueAreaHarvested))]
            data[(replaceIndex), valueAreaSown := get(valueAreaHarvested) *
                     ratio, with = FALSE]
            data[(replaceIndex), flagObsAreaSown := imputedObsFlag,
                 with = FALSE]
            data[(replaceIndex), flagMethodAreaSown := imputedMethodFlag,
                 with = FALSE]
            data[, replaceIndex := NULL]
        }
    }
    ## areaSownRatio must be >= 1, so fix any bad imputations
    data[areaSownRatio < 1, areaSownRatio := 1]
    
    ## If any values are still missing, impute areaSown with areaHarvested
    missingIndex = is.na(data[[valueAreaSown]])
    data[missingIndex, c(valueAreaSown) := get(valueAreaHarvested)]
    data[missingIndex, c(flagObsAreaSown) := get(flagObsAreaHarvested)]
    data[missingIndex, c(flagMethodAreaSown) := imputedMethodFlag]
}