Classification___Logistic___Results___Coefficients = function(Logistic){
  fit = Logistic$Best_Model

  #===========================================================================
  # glm
  #===========================================================================
  if("glm" %in% class(fit)){
    Logistic$Best_Coef = fit$coefficients







  #===========================================================================
  # glmnet
  #===========================================================================
  }else if("glmnet" %in% class(fit)){

    # Check if lambda.min is present
    if("lambda.min" %in% names(fit)){
      # lambda.min exists, use it to extract coefficients
      non_zero_coefs = coef(fit, s = Fit$lambda.min)
    } else {
      # lambda.min does not exist, use the first lambda in the sequence
      # Alternatively, you could use a specific lambda value of your choice
      non_zero_coefs = coef(fit, s = fit$lambda[1])
    }

    # Convert to a regular matrix if it is a sparse matrix
    if (class(non_zero_coefs) == "dgCMatrix") {
      non_zero_coefs_matrix = as.matrix(non_zero_coefs)
    } else {
      non_zero_coefs_matrix = non_zero_coefs
    }

    # Extract non-zero coefficients
    non_zero_coef_names = rownames(non_zero_coefs_matrix)[non_zero_coefs_matrix[, 1] != 0]
    non_zero_coef_values = non_zero_coefs_matrix[non_zero_coefs_matrix[, 1] != 0, 1]
    Logistic$Best_Coef = data.frame(names(non_zero_coef_values), non_zero_coef_values %>% unname)





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

    Logistic$Best_Coef = Fit_Coef





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

    Logistic$Best_Coef = Fit_Coef
  }

  # Combine as a list
  # Fit_Coef = list(Intercept = Fit_Intercept, Slope = Fit_Slope[Fit_Slope!=0], Combined = Fit_Coef)




  return(Logistic)
}
