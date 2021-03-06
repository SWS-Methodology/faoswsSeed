##' Get All Area Data
##' 
##' Description
##' 
##'
##' 
##' @export

getAreaSownData = function(){
  ## Setups    
  areaKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = areaVar,
                keys = getAllCountries()),
      Dimension(name = elementVar,
                keys = c(areaSownElementCode)),
      Dimension(name = itemVar,
                keys = getAllItemCPC()),
      Dimension(name = yearVar,
                keys = getAllYears())
    )
  )
  
  ## Pivot to vectorize yield computation
  newPivot = c(
    Pivoting(code = areaVar, ascending = TRUE),
    Pivoting(code = itemVar, ascending = TRUE),
    Pivoting(code = yearVar, ascending = FALSE),
    Pivoting(code = elementVar, ascending = TRUE)
  )
  
  ## Query the data
  query = GetData(
    key = areaKey,
    flags = TRUE,
    normalized = FALSE,
    pivoting = newPivot
  )
  
  query[, timePointYears := as.numeric(timePointYears)]
  setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
  query
}