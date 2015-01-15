##' Fill country specific seed rates
##' 
##' @param data The seed data.table, typically as produced by a call to
##' getAreaData.
##' @param countrySpecificData A data.table with data describing seed rates for
##' each country.
##' 
##' @return No value is returned.  Instead, seed rates (from
##' countrySpecificData) are appended onto data.
##' 
##' @export
##' 

fillCountrySpecificSeedRate = function(data,
    countrySpecificData = getCountrySpecificSeedRate()){
    ## Fill in the country Specific rates          
    data[countrySpecificData,
         `:=`(c("Value_seedRate", "flagObservationStatus_seedRate"),
              list(i.Value_seedRate, i.flagObservationStatus_seedRate)),
         allow.cartesian = TRUE]
    data
}