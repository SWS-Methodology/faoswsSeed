## Steps:
##
## (1) First get the area harvested/sown data using the R API (GetData)
##
## (2) Then get the seed rate data after loading them in to the data
##     base. (ReadDatatable).
##
## NOTE (Michael): The codes need to be converted to CPC from FCL.
##
##
## (3) Multiply the seed rate with the area harvested/sown in the
##     following year to get the seed used in the current year.
##
## (4) Save the data back.

suppressMessages({
  library(bit64)
  library(curl)
  library(faosws)
  library(faoswsUtil)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(igraph)
  library(plyr)
  library(dplyr)
  ##library(RJDBC)
  library(ggplot2)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(splines)
  library(faoswsImputation)
  library(faoswsEnsure)
  
})

library(faoswsSeed)

## We use bootstrapping to estimate variance, so we should set the seed here to
## allow for reproducible results. NO MORE!
#set.seed(12345)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
areaSownElementCode = "5025"
areaHarvestedElementCode = "5312"
seedElementCode = "5525"
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"


updateModel = TRUE

## set up for the test environment and parameters


library(faosws)

if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS <- ReadSettings("sws.yml")
  SetClientFiles(SETTINGS[["certdir"]])
  
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}



if(updateModel){
  seed = getOfficialSeedData()
  
  

  
  ## Seed imput data validation
  ## Ensure that the seed official values belong to the tha range (0, inf). 
  ensureValueRange(data = seed,
                   ensureColumn = "Value_measuredElement_5525",
                   min = 0,
                   max = Inf,
                   includeEndPoint = TRUE,
                   returnData = FALSE,
                   getInvalidData = FALSE)
  
  
  
## ensureFlagValidity(data = seed,
##                   flagObservationVar = "flagObservationStatus_measuredElement_5525",
##                     flagMethodVar = "flagMethod_measuredElement_5525",
##                     returnData = FALSE,
##                     getInvalidData = TRUE)
  
  
##Ensure   CorrectMissingValue not necessary because I pull only official data whose ObservationFlag is ""
  
  ensureCorrectMissingValue(data = seed,
                            valueVar = "Value_measuredElement_5525",
                            flagObservationStatusVar = "flagObservationStatus_measuredElement_5525",
                            missingObservationFlag = "M",
                            returnData = FALSE,
                            getInvalidData = TRUE)
  
  
  
  seed = removeCarryForward(data = seed,
                            variable = "Value_measuredElement_5525")
  seed = buildCPCHierarchy(data = seed, cpcItemVar = itemVar, levels = 3)
  
  
  
  
  
  area = getAllAreaData()
  
  areaConflict =areaRemoveZeroConflict(area,
                         value1= "Value_measuredElement_5025",
                         value2= "Value_measuredElement_5312",
                         observationFlag1= "flagObservationStatus_measuredElement_5025",
                         methodFlag1= "flagMethod_measuredElement_5025",
                         missingObservationFlag = "M",
                         missingMethodFlag = "u"
                         )
  
  areaPreProcessed= preProcessing(data= areaConflict,
                normalised = FALSE)
  
  ensureValueRange(data = areaPreProcessed,
                   ensureColumn = "Value_measuredElement_5312",
                   min = 0,
                   max = Inf,
                   includeEndPoint = TRUE,
                   returnData = FALSE,
                   getInvalidData = FALSE)
  
  
  ensureValueRange(data = areaPreProcessed,
                   ensureColumn = "Value_measuredElement_5025",
                   min = 0,
                   max = Inf,
                   includeEndPoint = TRUE,
                   returnData = FALSE,
                   getInvalidData = FALSE)
  
  
  
 imputedArea=imputeAreaSown(data = areaPreProcessed,
                 byKey= "geographicAreaM49")
 
 
 ## AreaSown must be greater then or at least equal to area harvested. Check that in the AreaSown column all
 ## the values are not lower than AreaHarvested
 
  
ensureNoConflictingAreaSownHarvested(data= imputedArea,
                                     valueColumn1= "Value_measuredElement_5025",
                                     valueColumn2=  "Value_measuredElement_5312",
                                      returnData = FALSE,
                                      normalised = FALSE,
                                      getInvalidData = FALSE)
  
  
 
climate= getWorldBankClimateData()
  
  
  
  


  
  
  
  finalModelData = mergeAllSeedData(seedData = seed, imputedArea, climate)
  finalModelData = finalModelData[Value_measuredElement_5525 > 1 &
                                      Value_measuredElement_5025 >1, ]
  ## We have to remove cases where we do not have temperature, as we cannot create
  ## a model when independent variables are missing.  The predictions would be NA
  ## anyways, and so we wouldn't be saving anything to the database if we left
  ## them in.
  finalModelData = finalModelData[!is.na(Value_wbIndicator_SWS.FAO.TEMP), ]
  
  # Michael's model
  seedLmeModel = 
    lmer(log(Value_measuredElement_5525) ~ Value_wbIndicator_SWS.FAO.TEMP +
           timePointYears + 
           (log(Value_measuredElement_5025)|cpcLvl3/measuredItemCPC:geographicAreaM49),
         data = finalModelData)

# Nata's model
# seedLmeModel = 
#   lmer(log(Value_measuredElement_5525 + 1) ~ Value_wbIndicator_SWS.FAO.TEMP +
#          timePointYears + 
#          log(Value_measuredElement_5025 + 1) +
#          (log(Value_measuredElement_5025 + 1)|cpcLvl3/measuredItemCPC:geographicAreaM49),
#        data = finalModelData)



# par(mfrow=c(1,1))
# qqnorm(residuals(lossLmeModel))
# qqline(residuals(lossLmeModel))
}

seed = getSelectedSeedData()

seed = removeCarryForward(data = seed, variable = "Value_measuredElement_5525")
seed = buildCPCHierarchy(data = seed, cpcItemVar = itemVar, levels = 3)

finalPredictData = mergeAllSeedData(seedData = seed, imputedArea, climate)
finalPredictData = finalPredictData[Value_measuredElement_5525 > 1 &
                                        Value_measuredElement_5025 > 1, ]
## We have to remove cases where we do not have temperature, as we cannot create
## a model when independent variables are missing.  The predictions would be NA
## anyways, and so we wouldn't be saving anything to the database if we left
## them in.
finalPredictData = finalPredictData[!is.na(Value_wbIndicator_SWS.FAO.TEMP), ]


## Impute selected data

finalPredictData[, predicted := exp(predict(seedLmeModel,
                                            newdata = finalPredictData,
                                            allow.new.levels = TRUE))]
# ggplot(finalPredictData, aes(x = Value_measuredElement_5525, y = predicted)) +
#   geom_point()


finalPredictData= preProcessing(data= finalPredictData,
                                normalised = FALSE)

finalPredictData=removeManuallyEstimation(finalPredictData,
                          valueVar= "Value_measuredElement_5525",
                          observationFlagVar= "flagObservationStatus_measuredElement_5525",
                          methodFlagVar= "flagMethod_measuredElement_5525")




finalPredictData=removeImputationEstimation(finalPredictData,
                       valueVar= "Value_measuredElement_5525",
                       observationFlagVar= "flagObservationStatus_measuredElement_5525",
                       methodFlagVar= "flagMethod_measuredElement_5525")




finalPredictData[(is.na(Value_measuredElement_5525) |
                  flagObservationStatus_measuredElement_5525 %in% c("M")) &
                 !is.na(predicted),
             `:=` (c("Value_measuredElement_5525",
                     "flagObservationStatus_measuredElement_5525",
                     "flagMethod_measuredElement_5525"),
                 list(predicted, "I", "e"))]

finalPredictData[, predicted := NULL]



##Output data validation 


ensureValueRange(data = finalPredictData,
                 ensureColumn = "Value_measuredElement_5525",
                 min = 0,
                 max = Inf,
                 includeEndPoint = TRUE,
                 returnData = FALSE,
                 getInvalidData = FALSE)


ensureFlagValidity(data = finalPredictData,
                   flagObservationVar = "flagObservationStatus_measuredElement_5525",
                   flagMethodVar = "flagMethod_measuredElement_5525",
                   returnData = FALSE,
                   getInvalidData = FALSE) 



finalPredictData[, .(geographicAreaM49, timePointYears, measuredItemCPC,
                     Value_measuredElement_5525, flagObservationStatus_measuredElement_5525, 
                     flagMethod_measuredElement_5525)] %>%
  mutate(timePointYears = as.character(timePointYears)) %>%
  filter(., (flagObservationStatus_measuredElement_5525 == "I" & 
               flagMethod_measuredElement_5525 == "e")) %>%
  ensureProtectedData(data = .,
                                         domain = "agriculture",
                                         dataset = "aproduction",
                                         normalised = FALSE,
                                         returnData = FALSE,
                                         getInvalidData = FALSE
                                         )



saveSeedData(data = finalPredictData)

"Module finished successfully"
