Classification___Multinomial___Results___Coefficients = function(fit){
  #===========================================================================
  # polr
  #===========================================================================
  if(class(fit)=="polr"){
    # Intercept
    Fit_Intercept = fit$zeta
    # Slope
    Fit_Slope = -fit$coefficients

    # Combined
    Fit_Coef = c(Fit_Intercept, Fit_Slope)
    Fit_Coef = data.frame(Coef = names(Fit_Coef), value = Fit_Coef)
    rownames(Fit_Coef) = NULL

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

  }

  # Combine as a list
  Fit_Coef = list(Intercept = Fit_Intercept, Slope = Fit_Slope[Fit_Slope!=0], Combined = Fit_Coef)

  return(Fit_Coef)
}
