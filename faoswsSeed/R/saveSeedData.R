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
    ## Save the data back
    SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
             dataset = slot(swsContext.datasets[[1]], "dataset"),
             data = data,
             normalized = FALSE)
}