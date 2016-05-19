##' Get All Countries
##' 
##' Description
##' 
##'
##' 
##' 

getAllCountries = function(){
    GetCodeList(domain = "agriculture",
                dataset = "aproduction",
                dimension = "geographicAreaM49")[type == "country", code]
}

