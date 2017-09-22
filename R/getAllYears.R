##' Get All Years
##' 
##' Description
##' 
##'
##' 
##' 

getAllYears = function(){
  getCompleteImputationKey("production")
  
  justForYear@dimensions$timePointYears@keys
}