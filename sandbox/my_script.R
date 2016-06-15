 xyplot(predicted~Value_measuredElement_5525|cpcLvl3, data=finalPredictData,
       auto.key=list(space="top", columns=length(unique(finalPredictData[,cpcLvl3])), 
       title="District", cex.title=1,
       lines=TRUE, points=FALSE)
          )
 
 
 xyplot(predicted~Value_measuredElement_5525, data=finalPredictData)
 
 xyplot(predicted~Value_measuredElement_5525, data=finalPredictData, groups= cpcLvl3, 
        auto.key = TRUE)
 
 index= finalPredictData[[measuredItemCPC=="0151"]]+
   
   
   finalPredictDataSubsetCommodity=finalPredictData[
        measuredItemCPC=="01510" |
        measuredItemCPC=="0151" |
        measuredItemCPC=="0111" |
        measuredItemCPC=="0113" |
        measuredItemCPC=="0115" |
        measuredItemCPC=="0116" |
        measuredItemCPC=="01802" |
        
      measuredItemCPC=="0117",]
 
 
 
 finalPredictDataPotatoes=finalPredictData[
   measuredItemCPC=="01510" ,]
 
 
 testPotatoes=finalPredictDataPotatoes[predicted>5000000,]
 
  
  xyplot(predicted~Value_measuredElement_5525|measuredItemCPC, data=finalPredictDataSubsetCommodity,
         auto.key=list(space="top", columns=length(unique(finalPredictDataSubsetCommodity[,cpcLvl3])), 
                       title="District", cex.title=1,
                       lines=TRUE, points=FALSE)
  )
  
  
  Michael PLOT (check potatoes Bielorussia)
  
  > library(lattice)
  > xyplot(Value_measuredElement_5025 + Value_measuredElement_5312 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > xyplot(Value_measuredElement_55525/Value_measuredElement_5025 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  Error in eval(expr, envir, enclos) : 
    object 'Value_measuredElement_55525' not found
  > xyplot(Value_measuredElement_5525/Value_measuredElement_5025 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > xyplot(log(Value_measuredElement_5525)/log(Value_measuredElement_5025) ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > xyplot(log(Value_measuredElement_5525)/log(Value_measuredElement_5312) ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > xyplot(Value_measuredElement_5525/Value_measuredElement_5312 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > finalModelData[geographicAreaM49 == "112" & measuredItemCPC == "01510"]
  
  hist(finalModelData[measuredItemCPC == "01510", Value_areaSownRatio])
  > xyplot(Value_measuredElement_5525/Value_measuredElement_5312 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > xyplot(Value_measuredElement_5525/Value_measuredElement_5025 ~timePointYears|geographicAreaM49 , data = finalPredictData[measuredItemCPC == "01510"])
  > 
    > 
    > finalModelData[geographicAreaM49 == "112" & measuredItemCPC == "01510"]
  
  
  
  Michael: check before save data 
  
  ensureProtectedData(data = finalPredictData[, c("geographicAreaM49", "timePointYears", "measuredItemCPC", "Value_measuredElement_5525", "flagObservationStatus_measuredElement_5525", "flagMethod_measuredElement_5525")],
                      +                     domain = "agriculture",
                      +                     dataset = "aproduction",
                      +                     flagObservationVar = "flagObservationStatus_measuredElement_5525",
                      +                     flagMethodVar = "flagMethod_measuredElement_5525",
                      +                     normalised = FALSE,
                      +                     returnData = FALSE,
                      +                     getInvalidData = FALSE
                      +                     )