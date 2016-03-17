##' Get Selected Seed Data
##' 
##' This function pulls the seed data from the SWS.
##' 
##' @param dataContext The DatasetKey, containing the relevant countries, years,
##'   commodities and elements for pulling the seed data.
##'   
##' @return The dataset with seed data.
##' 
##' @export 
##'   

getSelectedSeedData = function(dataContext){
    
    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )

    selectedSeed =
        GetData(key = dataContext, normalized = FALSE, pivoting = newPivot)
    selectedSeed[, timePointYears := as.numeric(timePointYears)]
    selectedSeed
}