##' Get All Years
##' 
##' Description
##' 
##'
##' 
##' 

getAllYears = function(){
    GetCodeList(domain = "agriculture",
                dataset = "aproduction",
                dimension = "timePointYears")[description != "wildcard", code]
}