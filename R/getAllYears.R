##' Get All Years
##' 
##' Description
##' 
##'
##' 
##' 

getAllYears = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "timePointYears")[description != "wildcard", code]
}