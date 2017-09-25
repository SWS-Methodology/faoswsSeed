##' Get All Years
##' 
##' Description
##' 
##'
##' 
##' 

getAllYears = function(){
  justForYear=getCompleteImputationKey("production")
  
  justForYear@dimensions$timePointYears@keys
}