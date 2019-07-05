timeHorizon <- 15
confidenceInterval <<- 1.96
tioBefore <<- 0.015
tioAfter <<- 0.022

modelNames = c("Model 1", "Model 2", "Model 3", "Model 4")
beta_0 = c(2.7594, 1.4212, 1.4258, 1.4212)
beta_t = c(-0.04314, -0.1779, -0.1795, -0.1779)
beta_t2 = c(-0.00093, -0.00044, -0.00044, -0.00044)
v_0 = c(0.3988, 0.09728, 0.1008, 0.09728)
cov1 = c(-0.00048, 0.000597, 0.000873, 0.000597)
v_t = c(0.000941, 0.000749, 0.000769, 0.000749)
v_e = c(0.01724, 0.01703, 0.01703, 0.01703)

REGRESSION_CONSTANTS = data.frame(modelNames, beta_0, beta_t, beta_t2, v_0,
                                  cov1, v_t, v_e)
#' @title FEV1 Projection Model 1
#' @description This is the basic model with only the baseline
#' FEV1 score
#' @param fev1_0 numeric: Baseline FEV1 score (L)
#' @param int_effect numeric: Effect of intervention on lung function (L)
#' @param tio string: is patient receiving tiotropium, "Yes" or "No"
#' @export

fev1_projection1 <- function(fev1_0, int_effect, tio="No"){

  constants = REGRESSION_CONSTANTS[1,]

  allYears = c(0:timeHorizon)
  futureYears = allYears[-1]
  fev1_avg <- c()
  vari <- c()
  obs <- fev1_0
  
  for (year in futureYears){
    
    t1 = year
    beta_t_x <- 0;
    beta_x_p <- 0;
    beta_x <- int_effect
    
    unconditional_mu = calculateUnconditionalMu(constants, beta_x, beta_t_x, t1, beta_x_p)
    sigmaMatrices = calculateSigmaMatrices(constants, t1, vari)
    fev1_avg[year] = calculateAverage(vari, unconditional_mu, obs, sigmaMatrices)
    vari[year] = calculateVariance(sigmaMatrices)
    
    if(tio=="Yes"){
      fev1_avg[year] = fev1_avg[year] + tioBefore*year
    }
  }
  
  fev1DataFrame = getFev1DataFrame(fev1_0, fev1_avg, vari, allYears)
  aa1 = getAA1(fev1DataFrame, fev1_0)
  bb1 = getBB1(fev1DataFrame, fev1_0)
  
  df_aa1 = list("df"=df, "aa1"=aa1, "bb1"=bb1, "options"=1)
  return(df_aa1)
}

#' @title FEV1 Projection Model 2
#' @description This is the complete model, including the O'Conner slope
#' @param fev1_0 numeric: Baseline FEV1 score (L)
#' @param int_effect numeric: Effect of intervention on lung function (L)
#' @param male 1 or 0, 1 = male, 0 = female
#' @param smoking 1 or 0, 1 = patient smokes, 0 = patient does not smoke
#' @param age integer: patient age
#' @param weight integer: patient weight in kilograms
#' @param height numeric: patient height in meters
#' @param oco numeric: O'Conner slope
#' @param tio string: is patient receiving tiotropium, "Yes" or "No"
#' @export

fev1_projection2 <- function(fev1_0, int_effect, male, smoking, age, weight, height, oco, tio="No"){
  
  allYears = c(0:timeHorizon)
  futureYears = allYears[-1]
  constants = REGRESSION_CONSTANTS[2,]
  gender <- male
  
  if (smoking==1){
    smo<-1
    int<-0
  } else if (smoking==0){
    smo<-0
    int<-0
  }
  
  fev1_avg <- c()
  vari <- c()
  obs <- fev1_0
  
  for (year in futureYears) {
    
    t1 <- year
    beta_x = betaXFunction(age, gender, weight, height, smo, int, oco=oco, int_effect=int_effect)
    beta_t_x = betaTXFunction(age, gender, weight, height, smo, int, oco, t1)
    beta_x_p = betaXFunction(age, gender, weight, height, smo=1, int=0, oco=oco, int_effect=0)
    beta_t_x_p = betaTXFunction(age, gender, weight, height, smo, int, oco, t1 = -1)
    
    unconditional_mu = calculateUnconditionalMu(constants, beta_x, beta_t_x, t1, beta_x_p)
    sigmaMatrices = calculateSigmaMatrices(constants, t1, vari)
    fev1_avg[year] = calculateAverage(vari, unconditional_mu, obs, sigmaMatrices)
    vari[year] = calculateVariance(sigmaMatrices)
    
    if(tio=="Yes"){
      fev1_avg[year] <- fev1_avg[year] + tioBefore*year
    }
  }
  
  fev1DataFrame = getFev1DataFrame(fev1_0, fev1_avg, vari, allYears)
  aa2 = getAA1(fev1DataFrame, fev1_0)
  bb2 = getBB1(fev1DataFrame, fev1_0)
  
  df_aa2 <- list("df"=df, "aa1"=aa2, "bb1"=bb2, "options"=2)
  return(df_aa2)
  
}
#' @title FEV1 Projection Model 3
#' @description This is the complete model, but without the O'Conner slope
#' @param fev1_0 numeric: Baseline FEV1 score (L)
#' @param int_effect numeric: Effect of intervention on lung function (L)
#' @param male 1 or 0, 1 = male, 0 = female
#' @param smoking 1 or 0, 1 = patient smokes, 0 = patient does not smoke
#' @param age integer: patient age
#' @param weight integer: patient weight in kilograms
#' @param height numeric: patient height in meters
#' @export

fev1_projection3 <- function(fev1_0, int_effect=0, male, smoking, age, weight, height){
  
  allYears = c(0:timeHorizon)
  futureYears = allYears[-1]
  constants = REGRESSION_CONSTANTS[3,]
  
  gender <- male
  
  if (smoking==1){
    smo<-1
    int<-0
  } else if (smoking==0){
    smo<-0
    int<-0
  }

  fev1_avg <- c()
  vari <- c()
  
  obs<-fev1_0
  
  for (year in futureYears) {
    
    t1 <- year
    
    beta_x <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*smo + -0.04159*int + -0.00837*age*height*height +
      0.02830*(1-smo) + int_effect;
    
    beta_t_x <- 0.002358*age*t1 + -0.00739*gender*t1 + 0.000127*weight*t1 +
      0.06680*height*t1 + 0.01565*height*height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 +
      -0.00094*age*height*height*t1;
    
    
    beta_x_p <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*(1) + -0.04159*(0) + -0.00837*age*height*height;
    
    beta_t_x_p <- 0.002358*age*(-1) + -0.00739*gender*(-1) + 0.000127*weight*(-1) +
      0.06680*height*(-1) + 0.01565*height*height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) +
      -0.00094*age*height*height*(-1);
    
    
    unconditional_mu <- c(
      mu_f = constants$beta_0 + 
        beta_x + 
        beta_t_x + 
        constants$beta_t*t1 + 
        constants$beta_t2*t1*t1,
      mu_0 = constants$beta_0 + 
        beta_x_p
    )
    
    sigmaMatrices = calculateSigmaMatrices(constants, t1, vari)
    fev1_avg[year] = calculateAverage(vari, unconditional_mu, obs, sigmaMatrices)
    vari[year] = calculateVariance(sigmaMatrices)
  }
  
  
  fev1_avg<-c(fev1_0, fev1_avg)
  vari<-c(0,vari)
  
  fev1_up<-fev1_avg+confidenceInterval*sqrt(vari)
  fev1_low<-fev1_avg-confidenceInterval*sqrt(vari)
  
  df <-data.frame(allYears, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Year", "FEV1", "variance", "FEV1_lower", "FEV1_upper")
  
  res <- df
  return(res)
  
}

#' @title FEV1 Projection Model 4
#' @description This is the extended model, with patient history of FEV1 score
#' @param fev1_0 numeric: Baseline FEV1 score (L)
#' @param fev1_prev numeric: FEV1 score in previous year (L)
#' @param int_effect numeric: Effect of intervention on lung function (L)
#' @param male 1 or 0, 1 = male, 0 = female
#' @param smoking 1 or 0, 1 = patient smokes, 0 = patient does not smoke
#' @param age integer: patient age
#' @param weight integer: patient weight in kilograms
#' @param height numeric: patient height in meters
#' @param oco numeric: O'Conner slope
#' @param tio string: is patient receiving tiotropium, "Yes" or "No"
#' @export
fev1_projection4 <- function(fev1_0, fev1_prev, int_effect, male, smoking, age, weight, height, oco, tio="No"){
  
  x <- c(-1:timeHorizon)
  
  gender = male
  
  if (smoking==1){
    smo<-1
    int<-0
  } else if (smoking==0){
    smo<-0
    int<-0
  }
  
  beta_0<-1.4212
  beta_t<--0.1779
  beta_t2<--0.00044
  v_0<-0.09728
  cov1<-0.000597
  v_t<-0.000749
  v_e<-0.01703
  
  fev1_avg <- c()
  vari <- c()
  
  obs<-c(fev1_prev,fev1_0)
  
  for (i in 1:timeHorizon)
  {
    t1 <- i
    
    beta_x = betaXFunction(age, gender, weight, height, smo, int, oco=oco, int_effect=int_effect)
    beta_t_x = betaTXFunction(age, gender, weight, height, smo, int, oco, t1)
    beta_x_p = betaXFunction(age, gender, weight, height, smo=1, int=0, oco=oco, int_effect=0)
    beta_t_x_p = betaTXFunction(age, gender, weight, height, smo=1, int=0, oco, t1 = -1)
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    unconditional_mu<-c(unconditional_mu, mu_p=beta_0 + beta_x_p + beta_t_x_p + beta_t*(-1) + beta_t2*(-1)*(-1))
    v_t_p<-v_0 + v_t + 2*(-1)*cov1 + v_e
    cov_0_p<-v_0 + (-1)*cov1
    cov_f_p<-v_0 + t1*cov1 + (-1)*cov1 + (-1)*t1*v_t
    cov_mat<-rbind(cbind(cov_mat,c(cov_f_p,cov_0_p)),c(cov_f_p,cov_0_p,v_t_p))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_prev, fev1_0, fev1_avg)
  vari<-c(0,0,vari)
  
  fev1_up<-fev1_avg+confidenceInterval*sqrt(vari)
  fev1_low<-fev1_avg-confidenceInterval*sqrt(vari)
  
  df<-data.frame(x, y=fev1_avg, fev1_low, fev1_up)
  names(df) <- c("Year", "FEV1", "FEV1_lower", "FEV1_upper")
  
  cv4<-sqrt(vari[3:13])/(fev1_avg[3:13]-fev1_prev)
  aa4<-rbind(fev1_avg[3:13], fev1_up[3:13], fev1_low[3:13], round(abs(cv4)*100,0))
  
  n_mean4<-(fev1_avg[13]-fev1_prev)/12*1000
  n_sd4<-((fev1_avg[13]-fev1_prev)/12-(fev1_low[13]-fev1_prev)/12)/confidenceInterval*1000
  bb4<-data.frame(round(pnorm(-40, n_mean4, n_sd4)*100,0))
  
  df_aa4 <- list("df"=df, "aa1"=aa4, "bb1"=bb4, "options"=4)
  return(df_aa4)
}