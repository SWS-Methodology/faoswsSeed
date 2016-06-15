autoFlagCorrection = function(data,
                              flagObservationStatusVar = "flagObservationtatus",
                              flagMethodVar = "flagMethod"){
  dataCopy = copy(data)
  
  ## Correction (1): (M, -) --> (M, u)
  correctionFilter =
    dataCopy[[flagObservationStatusVar]] == "M" &
    dataCopy[[flagMethodVar]] == "-"
  dataCopy[correctionFilter,
           `:=`(c(flagObservationStatusVar, flagMethodVar),
                c("M", "u"))]
  
  ## Correction (2): (E, t) --> (E, -)
  correctionFilter =
    dataCopy[[flagObservationStatusVar]] == "E" &
    dataCopy[[flagMethodVar]] == "t"
  dataCopy[correctionFilter,
           `:=`(c(flagObservationStatusVar, flagMethodVar),
                c("E", "-"))]
  
  ## Correction (3): (E, e) --> (I, e)
  correctionFilter =
    dataCopy[[flagObservationStatusVar]] == "E" &
    dataCopy[[flagMethodVar]] == "e"
  dataCopy[correctionFilter,
           `:=`(c(flagObservationStatusVar, flagMethodVar),
                c("I", "e"))]
  
  ## Correction (4): (E, p) --> (E, f)
  correctionFilter =
    dataCopy[[flagObservationStatusVar]] == "E" &
    dataCopy[[flagMethodVar]] == "p"
  dataCopy[correctionFilter,
           `:=`(c(flagObservationStatusVar, flagMethodVar),
                c("E", "f"))]
  
  dataCopy
}