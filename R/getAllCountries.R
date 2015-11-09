##' Get All Countries
##' 
##' Description
##' 
##'
##' 
##' 

getAllCountries = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "geographicAreaM49")[type == "country", code]
}