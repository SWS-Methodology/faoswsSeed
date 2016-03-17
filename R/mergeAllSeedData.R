##' Merge All Seed Data
##' 
##' This function merges the seed data data.table object with any additional
##' datasets, using the intersection of the columns as the merging key(s).
##' 
##' @param seedData The data.table object containing the seed data.
##' @param ... Additional data.table objects to be merged with seedData.
##' 
##' @return A data.table containing the result of the merges.
##' 
##' @export
##' 
##' @importFrom data.table setkeyv
##' 

mergeAllSeedData = function(seedData, ...){
    explanatoryData = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        data.table::setkeyv(x, keys)
        data.table::setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
        },
        x = explanatoryData, init = seedData
    )
}
