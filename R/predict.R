#' Predicts COPD exacerbations within the next year
#' @param patientData patient data matrix. Can have one or many patients in it
#' @param onePatient boolean: if true, look at first patient in matrix only
#' @param predictionModel integer: one of [1,2,3,4]
#' @return patientData with prediction
#' @examples
#' results <- predictFEV1(samplePatients)
#' @export
predictFEV1 <- function (patientData, onePatient=TRUE, predictionModel = 3){
  
  if(onePatient){
    patientData = patientData[1,]
  }
  
  predictionModels = c(1:4)
  
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()
  
  if (!(predictionModel %in% predictionModels)) 
    ArgumentCheck::addError(
      msg = "Prediction Model must be one of 1,2,3,4",
      argcheck = Check
    )
  
  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)
  
  dataParameters = names(patientData)
  if(predictionModel == 1) {
    parameters = c("fev1_0", "int_effect", "tio")
    if(matchParameters(dataParameters, parameters)) {
      results = fev1_projection1(fev1_0 = patientData$fev1_0,
                                       int_effect = patientData$int_effect,
                                       tio = patientData$tio)$df
      return(results)
    }
  } else if(predictionModel == 2) {
    parameters = c("fev1_0", "int_effect","male", "smoking", "age", "weight", "height", "oco", "tio")
    if(matchParameters(dataParameters, parameters)) {
      results_smoker = fev1_projection2(fev1_0 = patientData$fev1_0, 
                                        int_effect = patientData$int_effect,
                                        male = patientData$male,
                                        smoking = 1, 
                                        age = patientData$age,
                                        weight = patientData$weight, 
                                        height = patientData$height,
                                        oco = patientData$oco,
                                        tio = patientData$tio)
      results_smoker$Scenario = "Smoking"
      results_sustainedQuitter = fev1_projection2(fev1_0 = patientData$fev1_0, 
                                                  int_effect = patientData$int_effect,
                                                  male = patientData$male,
                                        smoking = 0, age = patientData$age,
                                        weight = patientData$weight, 
                                        height = patientData$height,
                                        oco = patientData$oco,
                                        tio = patientData$tio)
      results_sustainedQuitter$Scenario <- "QuitsSmoking"
      results <- rbind (results_smoker,  results_sustainedQuitter)
      return(results)
      
    } else {
      stop("Missing arguments for FEV1 Projection Model 2")
    }
  } else if(predictionModel == 3) {
    parameters = c("fev1_0", "int_effect","male", "smoking", "age", "weight", "height", "tio")
    if(matchParameters(dataParameters, parameters)) {
      results_smoker <- fev1_projection3(fev1_0 = patientData$fev1_0, 
                                         int_effect = patientData$int_effect,
                                       male = patientData$male, 
                                       smoking = 1, 
                                       age = patientData$age, 
                                       weight = patientData$weight, 
                                       height = patientData$height)
  
      results_smoker$Scenario <- "Smoking"
      results_sustainedQuitter <- fev1_projection3(fev1_0 = patientData$fev1_0, 
                                                   int_effect = patientData$int_effect,
                                                   male = patientData$male, 
                                                   smoking = 0, 
                                                   age = patientData$age, 
                                                   weight = patientData$weight, 
                                                   height = patientData$height)
  
      results_sustainedQuitter$Scenario <- "QuitsSmoking"
  
      results <- rbind (results_smoker,  results_sustainedQuitter)
      return(results)
    } else {
      print(dataParameters)
      print(parameters)
      stop("Missing arguments for FEV1 Projection Model 3")
    }
  } else if(predictionModel == 4) {
    parameters = c("fev1_0","fev1_prev", "int_effect","male", "smoking", 
                   "age", "weight", "height", "oco", "tio")
    if(matchParameters(dataParameters, parameters)) {
      results_smoker = fev1_projection4(fev1_0 = patientData$fev1_0, 
                                        fev1_prev = patientData$fev1_prev,
                                        int_effect = patientData$int_effect,
                                        male = patientData$male,
                                        smoking = 1, 
                                        age = patientData$age,
                                        weight = patientData$weight, 
                                        height = patientData$height,
                                        oco = patientData$oco,
                                        tio = patientData$tio)
      results_smoker$Scenario = "Smoking"
      results_sustainedQuitter = fev1_projection4(fev1_0 = patientData$fev1_0, 
                                                  fev1_prev = patientData$fev1_prev,
                                                  int_effect = patientData$int_effect,
                                                  male = patientData$male,
                                                  smoking = 0, 
                                                  age = patientData$age,
                                                  weight = patientData$weight, 
                                                  height = patientData$height,
                                                  oco = patientData$oco,
                                                  tio = patientData$tio)
      results_sustainedQuitter$Scenario <- "QuitsSmoking"
      results <- rbind (results_smoker,  results_sustainedQuitter)
      return(results)
      
    } else {
      print(dataParameters)
      print(parameters)
      stop("Missing arguments for FEV1 Projection Model 4")
    }
  }

}

#' Checks to make sure the parameters all match
#' @param dataParameters the parameters in the given data
#' @param parameters the parameters required for the model
#' @return TRUE if all the arguments match, else FALSE
matchParameters = function(dataParameters, parameters) {
  for(parameter in parameters) {
    if(!(parameter %in% dataParameters)) {
      return(FALSE)
    }
  }
  return(TRUE)
}



