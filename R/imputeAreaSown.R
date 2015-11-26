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
##' @param codeAreaHarvested The element code for the area harvested variable.
##' @param codeAreaSown The element code for the area sown variable.
##' @param imputedObsFlag The value to be assigned to the observation status
##' flag for imputed observations.
##' @param imputedMethodFlag The value to be assigned to the method flag for
##' imputed observations.
##' @param byKey Only used if imputationParameters is NULL (if
##' imputationParameters is not NULL, ensemble imputation is performed and the
##' byKey variable should be specified in this list).  This value
##' specifies how the mean seed to harvest ratio should be computed: by
##' country, globally, or in some other way.  Should be a column name of data,
##' or NULL (in which case a global mean is used).
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

imputeAreaSown = function(data, codeAreaHarvested = "5312",
                          codeAreaSown = "5212", imputedObsFlag = "I",
                          imputedMethodFlag = "e", byKey = NULL,
                          imputationParameters = NULL){
    
    ## Define column names
    valueAreaHarvested = paste0("Value_measuredElement_", codeAreaHarvested)
    flagObsAreaHarvested = paste0("flagObservationStatus_measuredElement_",
                                  codeAreaHarvested)
    flagMetAreaHarvested = paste0("flagMethod_measuredElement_",
                                  codeAreaHarvested)
    valueAreaSown = paste0("Value_measuredElement_", codeAreaSown)
    flagObsAreaSown = paste0("flagObservationStatus_measuredElement_", codeAreaSown)
    flagMetAreaSown = paste0("flagMethod_measuredElement_", codeAreaSown)
    
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
            ip = imputationParameters #shorter abbreviation (this is used alot)

            ## Define the areaSownRatio variable/flags:
            data[, Value_areaSownRatio :=
                     get(valueAreaSown)/get(valueAreaHarvested)]
            data[, flagObservationStatus_areaSownRatio :=
                     aggregateObservationFlag(get(flagObsAreaSown),
                                              get(flagObsAreaHarvested))]
            data[, flagMethod_areaSownRatio := "e"]
            ensureImputationInputs(data = data,
                                   imputationParameters = ip)
            remove0M(data = data, value = "Value_areaSownRatio",
                     flag = "flagObservationStatus_areaSownRatio")
            
            ## Impute by ensemble when data exists:
            keysForEnsemble = data[!is.na(Value_areaSownRatio), unique(get(ip$byKey))]
            filter = data[, geographicAreaM49 %in% keysForEnsemble]
            dataForEnsemble = data[filter, ]
            otherData = data[!filter, ]
            imputeVariable(dataForEnsemble, imputationParameters = ip)
            dataForEnsemble[, ensembleVariance := NULL]
            ## Impute by global mean otherwise
            otherData[!filter,
                 Value_areaSownRatio := mean(data[, Value_areaSownRatio],
                                             na.rm = TRUE)]
            newData = rbind(dataForEnsemble, otherData)
            
            ## Bind in new data:
            oldKey = key(data)
            tempKey = grep("^(?!flag|Value)", colnames(data), perl = TRUE, value = TRUE)
            setkeyv(data, tempKey)
            setkeyv(newData, tempKey)
            data[newData, Value_areaSownRatio := i.Value_areaSownRatio]
        } else { # Impute with simple mean
            data[, temporaryRatio := get(valueAreaSown) / 
                                     get(valueAreaHarvested)]
            data[, Value_areaSownRatio := mean(
                temporaryRatio[temporaryRatio < Inf], na.rm = TRUE),
                by = byKey]
            data[, temporaryRatio := NULL]
            data[, flagObservationStatus_areaSownRatio := "I"]
            data[, flagMethod_areaSownRatio := "e"]
        }
    } else { # If all ratios are missing, assume a ratio of 1
        data[, Value_areaSownRatio := 1]
        data[, flagObservationStatus_areaSownRatio := "I"]
        data[, flagMethod_areaSownRatio := "e"]
    }
    ## Value_areaSownRatio must be >= 1, so fix any bad imputations
    data[Value_areaSownRatio < 1, Value_areaSownRatio := 1]
    
    ## Update valueAreaSown with the computed Value_areaSownRatio
    data[, replaceIndex := is.na(get(valueAreaSown)) &
         !is.na(get(valueAreaHarvested))]
    data[(replaceIndex), valueAreaSown := get(valueAreaHarvested) *
             Value_areaSownRatio, with = FALSE]
    data[(replaceIndex), flagObsAreaSown := imputedObsFlag,
         with = FALSE]
    data[(replaceIndex), flagMetAreaSown := imputedMethodFlag,
         with = FALSE]
    data[, replaceIndex := NULL]
    
    ## If any values are still missing, impute areaSown with areaHarvested
    missingIndex = is.na(data[[valueAreaSown]])
    data[missingIndex, c(valueAreaSown) := get(valueAreaHarvested)]
    data[missingIndex, c(flagObsAreaSown) := get(flagObsAreaHarvested)]
    data[missingIndex, c(flagMetAreaSown) := imputedMethodFlag]
}