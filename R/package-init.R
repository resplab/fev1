#' @import dplyr
#' @import ggplot2
#' @importFrom stats dpois quantile rpois pnorm
#' @importFrom stringr str_replace
#' @importFrom plotly ggplotly 
NULL

#' Sample Patient Characteristics Inputs
#'
#' A dataset containing sample patient characteristics to run the prediction model
#'  variables are as follows:
#'
#' \itemize{
#'   \item ID. patient ID
#'   \item male. whether the patient is male (0,1)
#'   \item age. the age of the patient (40--90)
#'   \item smoker. whether the patient is currently a smoker (0,1)
#'   \item FEV1. Baseline forced expiratroy volume in 1 second in L (0--5)
#'   \item height. in m (1--2.2)
#'   \item weight. in kg (20--150)
#'   }

#' @docType data
#' @keywords datasets
#' @name samplePatients
#' @format A data frame with 2 rows and 6 variables
NULL
