##' Build CPC Hierarchy
##' 
##' Function the build CPC Hierarchy
##' 
##' @param data
##' @param cocItemVar
##' @param levels
##' 
##' @return modifies the data object
##' 
##' @export

buildCPCHierarchy = function(data, cpcItemVar, levels = 3){
    data[, `:=`(c(paste0("cpcLvl", 1:levels)),
                lapply(1:levels, FUN = function(x)
                    factor(substr(data[[cpcItemVar]], 1, x))))]
}