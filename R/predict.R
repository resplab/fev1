#' Predicts COPD exacerbations within the next year
#' @param patientData patient data matrix. Can have one or many patients in it
#' @return patientData with prediction
#' @examples
#' results <- predictFEV1(samplePatients)
#' @export
predictFEV1 <- function (patientData){

  results <- fev1_projection3(fev1_0 = patientData$FEV1, male = patientData$male, smoking = patientData$smoker, age = patientData$age, 
                              weight = patientData$weight, height = patientData$height)
  return(results)

}

