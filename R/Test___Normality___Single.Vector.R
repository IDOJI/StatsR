Test___Normality___Single.Vector = function(x.vec, outlier_method, alpha=0.05){
  # 🟥Alpha ############################################################################
  if(class(alpha)!="numeric"){
    stop("alpha should be numeric")
  }




  # 🟥Outliers ############################################################################
  Have_Outliers = SUB___Which.Outliers(x = x.vec, method = outlier_method) %>% length > 0



  # 🟥Setting for results ############################################################################
  n = length(x.vec)
  results.list = list(NA, NA, NA, NA, NA)
  names(results.list) = c("Results", "What.Test", "p-val", "is.Normal", "N_Obs")
  results.list$N_Obs = n





  test =
  results.list$`p-val`
  # 🟥Test Decision ############################################################################
  ## 🟧sample size : n < 4 ====================================================================
  if(n<4){
    results.list[[1]] = "Small Sample"
    results.list[[2]] = "Nonparametric needed"
    results.list[[3]] = 0
  ## 🟧MJB Test : superior to others in every aspect ====================================================================
  }else if(Have_Outliers){
    results.list[[1]] = Test___Normality___Single.Vector___MJBTest(data = x.vec)
    results.list[[2]] = results.list[[1]]$Test
    results.list[[3]] = results.list[[1]]$p_value
  }else{
    results.list[[1]] = shapiro.test(x.vec)
    results.list[[2]] = "Shapiro-Wilk"
    results.list[[3]] = results.list[[1]]$p.value
  }

  # else if(n < 2000){

  # ## 🟧sample size : n < 2000 ====================================================================
  # }else{
  #   results.list[[1]] = moments::agostino.test(x.vec)
  #   results.list[[2]] = "D'Agostino skewness"
  #   results.list[[3]] = results.list[[1]]$p.value
  # }


  results.list[[4]] = (results.list[[3]] > alpha)
  return(results.list)
}


#
#
# '
# # KS, SW, Lilliefors(LF), Anderson-Darling(AD) : aren't effective sample size less than 30
# # 1)
# # Non-parametric
# # 2)
# # Shapiro-Wilks : sample size < 2000 (not cover the very largest sample sizes)
# # Kolmogorove-Smirnov : sample size < 2000
# # The KS test is substantially less powerful for testing normality than other tests
# # the Power of Shapio-Wilks test is low for small sample size
# # 3)
# # For large data samples, the D’Agostino Pearson test has shown to be the appropriate test as it would provide sensitivity for all conditions.
# # see paper : Empirical Power Comparison Of Goodness of Fit Tests for Normality In The Presence of Outliers
#



