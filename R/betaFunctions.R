betaXFunction = function(age, gender, weight, height, smo, int, oco, int_effect){
  
  WEIGHT_CONST = -0.00011
  HEIGHT_CONST = -1.7603
  HEIGHT2_CONST = 1.8931
  AGE_HEIGHT_CONST = -0.00820
  INT_CONST = -0.04131
  SMO_CONST = -0.07722
  NON_SMO_CONST = 0.02735
  AGE_CONST = -0.00519
  GENDER_CONST = 0.4625
  OCO_CONST = 0.002613
  
  betaX = AGE_CONST*age + 
    GENDER_CONST*gender + 
    WEIGHT_CONST*weight + 
    HEIGHT_CONST*height + 
    HEIGHT2_CONST*height*height +
    SMO_CONST*smo + 
    INT_CONST*int + 
    OCO_CONST*oco + 
    AGE_HEIGHT_CONST*age*height*height +
    NON_SMO_CONST*(1-smo) + 
    int_effect;
  
  return(betaX)
  
}

betaTXFunction = function(age, gender, weight, height, smo, int, oco, t1){
  
  T1_WEIGHT_CONST = 0.000149
  T1_HEIGHT_CONST = 0.07413
  T1_HEIGHT2_CONST = 0.01139
  T1_AGE_HEIGHT_CONST = -0.00092
  T1_INT_CONST = -0.01002
  T1_SMO_CONST = -0.02579
  T1_AGE_CONST = 0.002313
  T1_GENDER_CONST = -0.00886
  T1_OCO_CONST = 0.000195
  
  betaTX <- T1_AGE_CONST*age*t1 + 
    T1_GENDER_CONST*gender*t1 + 
    T1_WEIGHT_CONST*weight*t1 +
    T1_HEIGHT_CONST*height*t1 + 
    T1_HEIGHT2_CONST*height*height*t1 + 
    T1_SMO_CONST*smo*t1 + 
    T1_INT_CONST*int*t1 + 
    T1_OCO_CONST*oco*t1 +
    T1_AGE_HEIGHT_CONST*age*height*height*t1;
  
  return(betaTX)
  
}