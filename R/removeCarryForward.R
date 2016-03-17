##' Remove Carry Forward
##' 
##' In the old SWS, estimates were occassionally "carried forward", i.e. the 
##' estimate for the current year was just a copy of the estimate for the 
##' previous year.  This function removes such estimates from the dataset.
##' 
##' Note: I'm not sure if this function is still relevant, as all "trended" 
##' (i.e. carried forward) values from the old system were NOT copied to the new
##' system.  However, before removing this function, we should confirm that it 
##' is not needed.
##' 
##' @param data The data.table containing the seed data.  It should have columns
##'   geographicAreaM49 and measuredItemCPC.
##' @param variable The column name of data which contains the values that
##'   should be checked.
##'   
##' @return The data.table with the "carried forward" values removed.
##'   

removeCarryForward = function(data, variable){
    data[, variance := var(.SD[[variable]], na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(.SD[[variable]]),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}
