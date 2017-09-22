##' Get Official Seed Data
##' 
##' 
##' 
##' @export

getOfficialSeedData = function(){
 
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "aproduction",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = getAllCountries()),
            Dimension(name = elementVar,
                      keys = seedElementCode),
            Dimension(name = itemVar,
                      keys = getAllItemCPC()),
            Dimension(name = yearVar,
                      keys = getAllYears())
        )
    )

    ## Pivot to vectorize yield computation
    seedPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )

    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    
    
    ##  Function to pull only official data 
    ##  seedQuery[seedQuery[[paste0(flagObsPrefix, seedElementCode)]] == "", ]
    
    ##  OnlyProtected dtata 
    
    protectedFlag <- flagValidTable[flagValidTable$Protected == TRUE,] %>%
    .[, flagCombination := paste(flagObservationStatus, flagMethod, sep = ";")]
    
    col_keep <- names(seedQuery) %>%
      .[.!="flagCombination"]
    
    ## subset to protected flags
    seedQuery <-
      seedQuery[, flagCombination := paste(flagObservationStatus_measuredElement_5525, flagMethod_measuredElement_5525, sep = ";")] %>%
      merge(., protectedFlag, by = "flagCombination") %>%
      filter(Protected == TRUE) %>%       # only keep protected values
      select_(.dots = col_keep)
    
    
    
}