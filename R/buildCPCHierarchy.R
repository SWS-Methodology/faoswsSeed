##' Build CPC Hierarchy
##' 
##' This function creates a CPC hierarchy by taking the substring of the CPC 
##' code, up to "levels" number of digits.  For example, wheat (0111) would be 
##' in the group 011 if levels=3 and group 0 if levels=1.  Note that
##' 
##' @param data A data.table containing a column with CPC codes.
##' @param cpcItemVar Character.  The column name of data which corresponds to 
##'   the item/CPC code (for example, "measuredItemCPC").
##' @param levels How many CPC levels should be constructed?
##'   
##' @return Modifies the data object by adding cpcLvl columns (there will be
##'   "levels" columns added).
##'   
##' @export

buildCPCHierarchy = function(data, cpcItemVar, levels = 3){
    data[, `:=`(c(paste0("cpcLvl", 1:levels)),
                lapply(1:levels, FUN = function(x)
                    factor(substr(data[[cpcItemVar]], 1, x))))]
}
