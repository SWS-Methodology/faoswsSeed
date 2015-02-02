##' Save Seed Data
##' 
##' This function takes the a seed data dataset and saves it back to the
##' database.
##' 
##' @param data The data.table object containing the seed data to be written to
##' the database.
##' 
##' @return No R objects are returned, as this functions purpose is solely to
##' write to the database.
##' 
##' @export
##' 

saveSeedData = function(data){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    
    ## Remove columns to match the database
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears", "Value_measuredElement_5212",
                        "flagObservationStatus_measuredElement_5212",
                        "flagMethod_measuredElement_5212",
                        "Value_measuredElement_5312",
                        "flagObservationStatus_measuredElement_5312",
                        "flagMethod_measuredElement_5312",
                        "Value_measuredElement_5525",
                        "flagObservationStatus_measuredElement_5525",
                        "flagMethod_measuredElement_5525")
    data = data[, requiredColumns, with = FALSE]
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Missing required columns, so data cannot be saved!  Missing:\n",
             paste0(missingColumns, collapse = "\n"))
    
    ## Filter the data by removing any invalid date/country combinations
    areaValidRange = GetCodeList(domain = "agriculture",
                                 dataset = "agriculture",
                                 dimension = "geographicAreaM49")
    cleanDates = function(date){
        date = lapply(date, function(x) ifelse(is.null(x), NA, x))
        do.call("c", date)
    }
    areaValidRange[, startDate := cleanDates(startDate)]
    areaValidRange[, endDate := cleanDates(endDate)]
    setnames(areaValidRange, old = "code", new = "geographicAreaM49")
    data = merge(data, areaValidRange, by = "geographicAreaM49", all.x = TRUE)
    data[, date := as.Date(paste0(data$timePointYears, "-01-01",
                                  format = "%Y-%m-%d"))]
    data = data[is.na(startDate) | date > startDate, ]
    data = data[is.na(endDate) | date < endDate, ]
        
    ## Save the data back
    SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
             dataset = slot(swsContext.datasets[[1]], "dataset"),
             data = data,
             normalized = FALSE)
}