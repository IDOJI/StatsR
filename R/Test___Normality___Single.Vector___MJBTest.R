Test___Normality___Single.Vector___MJBTest = function(data){
  # the Modified Jarque-Bera
  # 🟥Install and load required packages ============================================
  if(!require('moments')){
    install.packages('moments')
  }
  library(moments)



  # 🟥Compute MAD : $\phi^2$ =============================================
  abs_median = abs(data - median(data)) %>% median
  phi = MAD = 1.4826 * abs_median
  phi_3 = phi^3
  phi_4 = phi^4



  # 🟥Compute Moments ====================================================
  n = length(data)
  mu_3_hat = ( (1/n) * (data - mean(data))^3 ) %>% sum()
  mu_4_hat = ( (1/n) * (data - mean(data))^4 ) %>% sum()


  # 🟥Compute MJB =========================================================
  The_1st_Term = (mu_3_hat/phi_3)^2
  The_2nd_Term = (mu_4_hat/phi_4 - 3)^2
  MJB_statistic = (n/6) * (The_1st_Term + (1/36) * The_2nd_Term)


  # 🟥p-value =========================================================
  p_value = pchisq(MJB_statistic, df = 2, lower.tail = F)



  # 🟥Results =========================================================
  results = data.frame(Test = "the Modified Jarque-Bera",
                       MJB_statistic,
                       p_value)
  return(results)
}


