Classification___Logistic___Ordinal___MLE = function(X_Train,
                                                        y_Train,
                                                        X_Test=NULL,
                                                        y_Test=NULL,
                                                        y_varname=NULL,
                                                        x_varname=NULL,
                                                        #=======================================
                                                        link = c("logistic", "probit", "loglog", "cloglog", "cauchit"),
                                                        #=======================================
                                                        AUC_in_Legend = T,
                                                        path_Export=NULL,
                                                        ...){
  #=============================================================================
  # arguments
  #=============================================================================
  link_choices = c("logistic", "probit", "loglog", "cloglog", "cauchit")
  link = match.arg(tolower(link), link_choices)







  #=============================================================================
  # Data combining
  #=============================================================================
  y_Train = y_Train %>% data.frame() %>% setNames(y_varname)
  Binded_Data = bind_cols(y_Train, X_Train) %>% as_tibble






  #=============================================================================
  # Install.pacakges
  #=============================================================================
  install_packages("MASS")






  #=============================================================================
  # Fit proportional odds model
  #=============================================================================
  fit = MASS::polr(SUB___as.formula(y_varname, x_varname), data = Binded_Data, Hess=TRUE, method = link) %>% suppressWarnings()







  #=============================================================================
  # Extract results
  #=============================================================================
  Results = Classification___Logistic___Results(fit,
                                                   Best_alpha=NULL,
                                                   X_Test,
                                                   y_Test,
                                                   x_varname,
                                                   y_varname,
                                                   AUC_in_Legend,
                                                   path_Export)






  #=============================================================================
  # interpretaion warning
  #=============================================================================
  # 음수 부호가 디폴트 모델이므로, 해석에 주의
  # warning(paste0("Since polr uses the following logit ", "logit[P(Y <= j | X = x_i)] = a_j - bx_i", " the interpretation of the coefficiets should be careful!"))





  #=============================================================================
  # return results
  #=============================================================================
  return(Results)
}
