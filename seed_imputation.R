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
  seed = removeCarryForward(data = seed,
                            variable = "Value_measuredElement_5525")
  seed = buildCPCHierarchy(data = seed, cpcItemVar = itemVar, levels = 3)
  
  area = getAllAreaData()
  imputeAreaSown(data = area)
  
  climate = getWorldBankClimateData()
  
  finalModelData = mergeAllSeedData(seedData = seed, area, climate)
  finalModelData = finalModelData[Value_measuredElement_5525 > 1 &
                                      Value_measuredElement_5025 > 1, ]
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



seed = removeCarryForward(data = seed, variable = "Value_measuredElement_5525")
seed = buildCPCHierarchy(data = seed, cpcItemVar = itemVar, levels = 3)

finalPredictData = mergeAllSeedData(seedData = seed, area, climate)
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

finalPredictData[(is.na(Value_measuredElement_5525) |
                  flagObservationStatus_measuredElement_5525 %in% c("E", "I", "T")) &
                 !is.na(predicted),
             `:=` (c("Value_measuredElement_5525",
                     "flagObservationStatus_measuredElement_5525",
                     "flagMethod_measuredElement_5525"),
                 list(predicted, "I", "e"))]

finalPredictData[, predicted := NULL]

saveSeedData(data = finalPredictData)

"Module finished successfully"
