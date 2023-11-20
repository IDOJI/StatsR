Classification___Logistic___Results___Coefficients = function(Logistic){
  fit = Logistic$Best_Model


  #===========================================================================
  # glm
  #===========================================================================
  if("glm" %in% class(fit)){
    Logistic$Best_Model_NonZeroCoefs = fit$coefficients




  #===========================================================================
  # glmnet
  #===========================================================================
  }else if("glmnet" %in% class(fit)){

    # 모델 계수 추출
    Coef.mat = coef(fit, s = fit$lambda)

    # 0이 아닌 계수만 추출 (절편 제외)
    Logistic$Best_Model_NonZeroCoefs = Coef.mat[Coef.mat[,1] != 0, , drop = FALSE] %>% as.matrix %>% as.data.frame
    Logistic$Best_Model_NonZeroCoefs = data.frame(rownames(Logistic$Best_Model_NonZeroCoefs), Logistic$Best_Model_NonZeroCoefs)




  #===========================================================================
  # polr
  #===========================================================================
  }else if(class(fit)=="polr"){
    # Intercept
    Fit_Intercept = fit$zeta

    # Slope
    Fit_Slope = -fit$coefficients

    # SE
    Fit_deviance = fit$deviance
    fit
    # Combined
    Fit_Coef = c(Fit_Intercept, Fit_Slope)
    Fit_Coef = data.frame(Coef = names(Fit_Coef), value = Fit_Coef)
    rownames(Fit_Coef) = NULL

    Logistic$Best_Model_NonZeroCoefs = Fit_Coef





  #===========================================================================
  # Ordinal Net
  #===========================================================================
  }else if(class(fit) == "ordinalNet"){
    Fit_Coef = fit$coefs
    which_intercepts = grep("Intercept", colnames(Fit_Coef))

    # Intercept
    Fit_Intercept = Fit_Coef[, which_intercepts] %>% unlist
    # Slope
    Fit_Slope = Fit_Coef[,-which_intercepts] %>% unlist

    # Combined
    Fit_Coef = data.frame(Coef = colnames(Fit_Coef), Values = Fit_Coef %>% as.vector)
    Fit_Coef = Fit_Coef %>% filter(Values!=0)

    Logistic$Best_Model_NonZeroCoefs = Fit_Coef




  #===========================================================================
  # grpreg
  #===========================================================================
  }else if("grpreg" %in% class(Logistic$Best_Model)){

    Coef = coef(Logistic$Best_Model, s = Logistic$Best_Model$cv_fit$lambda.min)

    # Filter out the non-zero coefficients
    NonZero_Coef = Coef[Coef != 0]
    NonZero_Coef = data.frame(names(NonZero_Coef), NonZero_Coef)
    rownames(NonZero_Coef) = NULL
    Logistic$Best_Model_NonZeroCoefs = NonZero_Coef

  }else{
    stop("Coef????????")
  }







  return(Logistic)
}
