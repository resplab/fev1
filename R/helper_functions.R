
getFev1DataFrame = function(fev1_0, fev1_avg, vari, years){
  fev1_avg = c(fev1_0, fev1_avg)

  vari = c(0,vari)

  fev1_up = fev1_avg + confidenceInterval*sqrt(vari)
  fev1_low = fev1_avg - confidenceInterval*sqrt(vari)

  df = data.frame(years, fev1_avg, vari, fev1_low, fev1_up)
  names(df) = c("Year", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
  return(df)
}

getAA1 = function(fev1DataFrame, fev1_0) {
  cv1 = sqrt(fev1DataFrame$vari[2:12]) / (fev1DataFrame$FEV1[2:12]-fev1_0)
  aa1 = rbind(fev1DataFrame$FEV1[2:12], 
              fev1DataFrame$fev1_up[2:12], 
              fev1DataFrame$fev1_low[2:12], 
              round(abs(cv1)*100, 0))
  return(aa1)
}

getBB1 = function(fev1DataFrame, fev1_0) {
  n_mean1 = (fev1DataFrame$FEV1[12]-fev1_0)/timeHorizon*1000
  n_sd1 = ((fev1DataFrame$FEV1[12]-fev1_0)/timeHorizon - 
             (fev1DataFrame$fev1_low[12]-fev1_0)/timeHorizon)/confidenceInterval*1000
  bb1 = data.frame(round(pnorm(-40, n_mean1, n_sd1)*100, 0))
  return(bb1)
}

calculateSigmaMatrices = function(constants, t1, vari){
  v_t_f<- constants$v_0 + t1^2*constants$v_t + 2*t1*constants$cov1
  v_t_0<- constants$v_0 + constants$v_e
  cov_f_0<- constants$v_0 + t1*constants$cov1
  cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
  
  sigma_11<-as.matrix(cov_mat[1,1])
  sigma_12<-as.matrix(t(cov_mat[1,-1]))
  sigma_21<-as.matrix(cov_mat[-1,1])
  sigma_22<-as.matrix(cov_mat[-1,-1])
  
  sigmaMatrices = list(sigma_11 = sigma_11, 
                       sigma_12 = sigma_12,
                       sigma_21 = sigma_21,
                       sigma_22 = sigma_22)
  return(sigmaMatrices)
}

calculateAverage = function(vari, unconditional_mu, obs, sigmaMatrices){
  average = unconditional_mu[1] + 
    sigmaMatrices$sigma_12%*%solve(sigmaMatrices$sigma_22)%*%(obs-unconditional_mu[-1])
  return(average)
}

calculateVariance = function(sigmaMatrices) {
  variance = sigmaMatrices$sigma_11 - 
    sigmaMatrices$sigma_12%*%solve(sigmaMatrices$sigma_22)%*%sigmaMatrices$sigma_21
  return(variance)
}

calculateUnconditionalMu = function(constants, beta_x, beta_t_x, t1, beta_x_p){
  unconditional_mu <- c(
    mu_f = constants$beta_0 + 
      beta_x + 
      beta_t_x + 
      constants$beta_t*t1 + 
      constants$beta_t2*t1*t1,
    mu_0 = constants$beta_0 + beta_x_p
  )
  return(unconditional_mu)
}
