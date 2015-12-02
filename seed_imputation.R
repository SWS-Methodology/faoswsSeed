## Steps:
##
## (1) First get the area harvested/sown data using the R API (GetData)
##
## (2) Then get the seed rate data after loading them in to the data
##     base. (GetTableData).
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
  library(RJDBC)
  library(ggplot2)
})


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
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == "") {
  if(Sys.info()[7] == "Golini"){ # Nata's work computer
    SetClientFiles(dir = "~/R certificate files/Production/")
    files = dir("~/GitHub/faoswsSeed/R", full.names = TRUE)
    # token = "7823c00b-b82e-47bc-8708-1be103ac91e4" # Michael's token
    # token = "95d4f013-3ef3-44c6-99b1-cb431f2b7ae8" # Josh's token
    token = "d986d102-c3ea-4aa8-8da8-9355edc67fe0" # Nata's token 
  } else {
    stop("User not yet implemented!")
  }
  
  GetTestEnvironment(
    ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = token
  )  
  
  sapply(files, source)
}



if(updateModel){
  finalModelData = 
{
  seed =
    getOfficialSeedData() %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3)
  
  area =
    getAllAreaData() %>%
    imputeAreaSown(data = .)
  
  climate = getWorldBankClimateData()
  
} %>%
  mergeAllSeedData(seedData = seed, area, climate) %>%
  .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]%>%
  ## We have to remove cases where we do not have temperature, as we cannot create
  ## a model when independent variables are missing.  The predictions would be NA
  ## anyways, and so we wouldn't be saving anything to the database if we left
  ## them in.
  .[!is.na(Value_wbIndicator_SWS.FAO.TEMP), ]
  
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


finalPredictData = 
{
  if(!updateModel){
  
    area =
      getAllAreaData() %>%
      imputeAreaSown(data = .)
    
    climate = getWorldBankClimateData()
  }
  
  seed =
    getSelectedSeedData() %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3)
  
} %>%
  mergeAllSeedData(seedData = seed, area, climate) %>%
  .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]%>%
  ## We have to remove cases where we do not have temperature, as we cannot create
  ## a model when independent variables are missing.  The predictions would be NA
  ## anyways, and so we wouldn't be saving anything to the database if we left
  ## them in.
  .[!is.na(Value_wbIndicator_SWS.FAO.TEMP), ]



## Impute selected data

finalPredictData[, predicted := exp(predict(seedLmeModel, newdata = finalPredictData,
                                              allow.new.levels = TRUE))]

finalPredictData[(is.na(finalPredictData[["Value_measuredElement_5525"]]) |
                    finalPredictData[["flagObservationStatus_measuredElement_5525"]] %in% c("E", "I", "T")) &
               !is.na(predicted),
             `:=`(c("Value_measuredElement_5525", "flagObservationStatus_measuredElement_5525",
                    "flagMethod_measuredElement_5525"),
                 list(predicted, "I", "e"))]

finalPredictData[, predicted := NULL]

saveSeedData(data = finalPredictData)
