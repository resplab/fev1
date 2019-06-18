#' Predicts COPD exacerbations within the next year
#' @param patientData patient data matrix. Can have one or many patients in it
#' @return patientData with prediction
#' @examples
#' results <- predictFEV1(samplePatients)
#' @export
predictFEV1 <- function (patientData){

  results_smoker <- fev1_projection3(fev1_0 = patientData$FEV1, male = patientData$male, smoking = 1, age = patientData$age, 
                              weight = patientData$weight, height = patientData$height)
  
  
  results_smoker$Scenario <- "Smoking"
  results_sustainedQuitter <- fev1_projection3(fev1_0 = patientData$FEV1, male = patientData$male, smoking = 0, age = patientData$age, 
                                      weight = patientData$weight, height = patientData$height)
  
  results_sustainedQuitter$Scenario <- "QuitsSmoking"
  
  results <- rbind (results_smoker,  results_sustainedQuitter)
  return(results)

}

