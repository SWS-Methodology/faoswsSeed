##' Fill general seed rate
##' 
##' This function updates missing values of the Value_seedRate column in data.
##' If the value is missing, it is replaced with the commodity value.
##' 
##' @param data The data.table object containing the seed data, typically as
##' produced via getAreaData.
##' @param generalSeedData A data.table containing seeding data specific to
##' each commodity, typically as produced by getCountryGeneralSeedRate.
##' 
##' @return No object is returned.  Instead, the underlying data object is
##' modified.
##' 
##' @export
##' 

fillGeneralSeedRate = function(data,
                               generalSeedData = getCountryGeneralSeedRate()){
    ## fill in the general rates
    okey = key(data)
    setkeyv(data, key(generalSeedData))
    data[is.na(Value_seedRate), ][generalSeedData,
                                  `:=`(c("Value_seedRate"),
                                       list(i.Value_seedRate)),
                                  allow.cartesian = TRUE]
    setkeyv(data, okey)
}
