##' Check method flag
##' 
##' Method flags in the SWS are only allowed to take certain values.  This
##' function ensures that the supplied flags are all valid.
##' 
##' @param flag A character (or character vector) of flag values to test for
##' validity.
##' 
##' @return A logical vector of the same length as flag indicating if the flags
##' are valid (TRUE = valid).
##' 
##' @export
##' 

checkMethodFlag = function(flag){
    flag %in% c("-", "q", "p", "h", "c", "b", "i", "s", "t", "e", "f",
                "n", "u")
}