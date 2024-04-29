# 🟥 test___normality #############################################################################
test___normality = function(df,
                            group_var=NULL,
                            response_var=NULL,
                            outlier_method = c("IQR"),
                            alpha = 0.05,
                            p.adjust.method = c("bonferroni", "holm", "hochberg", "hommel","BH", "fdr", "BY", "SidakSS", "SidakSD", "ABH","TSBH", "none"),
                            path_save){
  ## 🟧 Test =================================================================================
  if(is.null(group_var)){

    result = test___normality___single.vector(df[[response_var]], outlier_method = outlier_method, alpha = alpha)

  }else{

    result = test___normality___group(df,
                                      group_var,
                                      response_var,
                                      outlier_method,
                                      alpha,
                                      p.adjust.method)

  }




  ## 🟧 Histogram + Density + QQplot ########################################################################################################
  plots.list = list()
  # Combined Group
  plots.list$hist_combined = ggplot___histogram(df = df,
                                                x = response_var,
                                                group_var = group_var,
                                                group_combined = T,
                                                density = T,
                                                same_colors_density = F,
                                                path_save = path_save)

  # Each group differently
  plots.list$hist_eachgroup = ggplot___histogram(df = df,
                                                 x = response_var,
                                                 group_var = group_var,
                                                 group_combined = F,
                                                 same_colors_density = F,
                                                 density = T,
                                                 path_save = path_save)



  # QQ plot
  plots.list$qqplot = ggplot___QQplot(df = df, x = response_var, group_var = group_var, path_save = path_save)



  ## 🟧 Combine results #######################################################################################################
  combined.list = list(test_result = result, plot = plots.list)



  cat("\n", crayon::green("Testing"), crayon::red("Normality"), crayon::green("is done!"),"\n")
  return(combined.list)
}





# 🟥 test___normality___group #############################################################################
test___normality___group = function(df,
                                    group_var,
                                    response_var,
                                    outlier_method,
                                    alpha,
                                    p.adjust.method){
  ## 🟧 Test for groups #############################################################################
  # 데이터를 group_var로 그룹화하고 response_var에 대해 함수 적용, 결과를 리스트로 만들기
  results_list <- df %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      NormalityTest = list(test___normality___single.vector(!!sym(response_var))),
      .groups = 'drop'
    ) %>%
    deframe()  # 데이터 프레임의 열을 리스트로 변환



  ## 🟧 Combine results #############################################################################
  # results_list를 데이터 프레임으로 변환
  results_df <- map_df(names(results_list), function(group_name) {
    data <- results_list[[group_name]]
    tibble(
      Group = group_name,
      n_obs = data$n_obs,
      # W = data$test_result$W,
      p_value = data$`p-value`,
      what_test = data$what_test,
      is_normal = data$is.normal
    )
  }, .id = "Group") %>%
    ccbind(data.frame(group = group_var), .) %>%
    ccbind(data.frame(response = response_var), .) %>%
    mutate(Group = names(results_list)) %>%
    as_tibble()



  ## 🟧 adjust p-vals #############################################################################
  results_df_2 = results_df %>%
    cbind(sub___p.adjust(p.values = results_df$p_value, method = p.adjust.method, alpha = alpha, only.return.p.vals = F)) %>%
    as_tibble() %>%
    dplyr::select(-p_value, -is_normal)



  ## 🟧 is.normal #############################################################################
  results_df_2$is.normal = results_df_2$adj.p.values > alpha

  return(results_df_2)
}


# 🟥 test___normality___single.vector #############################################################################
test___normality___single.vector = function(x.vec, outlier_method = "IQR", alpha=0.05){
  ## 🟧Alpha ###################################################################################
  if(class(alpha)!="numeric"){
    stop("alpha should be numeric")
  }


  ## 🟧Outliers #################################################################################
  # x.vec = data %>%
  #   dplyr::filter(Species == "virginica") %>%
  #   pull(!!response_var)
  # hist(x.vec)
  Have_Outliers = sub___which.outliers(x = x.vec, method = outlier_method) %>% length > 0




  ## 🟧Setting for results ############################################################################
  n = length(x.vec)
  results.list = list(NA, NA, NA, NA, NA)
  names(results.list) = c("test_result", "what_test", "p-value", "is.normal", "n_obs")
  results.list$n_obs = n




  ## 🟧Test Decision ############################################################################
  if(n<4){
    ### 🟨 sample size : n < 4 ====================================================================
    results.list[[1]] = "Small Sample"
    results.list[[2]] = "Nonparametric needed"
    results.list[[3]] = 0
  }else if(Have_Outliers){
    ### 🟨 MJB Test : superior to others in every aspect ====================================================================
    results.list[[1]] = mjb.test(x.vec)
    results.list[[2]] = results.list[[1]]$Test
    results.list[[3]] = results.list[[1]]$p_value
  }else{
    ### 🟨 Shapiro ====================================================================
    results.list[[1]] = shapiro.test(x.vec)
    results.list[[2]] = "Shapiro-Wilk"
    results.list[[3]] = results.list[[1]]$p.value
  }

  # else if(n < 2000){

  # ## 🟧sample size : n < 2000
  # }else{
  #   results.list[[1]] = moments::agostino.test(x.vec)
  #   results.list[[2]] = "D'Agostino skewness"
  #   results.list[[3]] = results.list[[1]]$p.value
  # }


  ## 🟧 Significance =================================================
  results.list[[4]] = (results.list[[3]] > alpha)




  return(results.list)
}

# 🟥 mjb.test #############################################################################
mjb.test = function(vector){
  # the Modified Jarque-Bera
  ## 🟧 Install and load required packages ============================================
  if(!require('moments')){
    install.packages('moments')
  }
  library(moments)



  ## 🟧 Compute MAD : $\phi^2$ =============================================
  abs_median = abs(vector - median(vector)) %>% median
  phi = MAD = 1.4826 * abs_median
  phi_3 = phi^3
  phi_4 = phi^4



  ## 🟧 Compute Moments ====================================================
  n = length(vector)
  mu_3_hat = ( (1/n) * (vector - mean(vector))^3 ) %>% sum()
  mu_4_hat = ( (1/n) * (vector - mean(vector))^4 ) %>% sum()


  ## 🟧 Compute MJB =========================================================
  The_1st_Term = (mu_3_hat/phi_3)^2
  The_2nd_Term = (mu_4_hat/phi_4 - 3)^2
  MJB_statistic = (n/6) * (The_1st_Term + (1/36) * The_2nd_Term)


  ## 🟧 p-value =========================================================
  p_value = pchisq(MJB_statistic, df = 2, lower.tail = F)



  ## 🟧 Results =========================================================
  results = data.frame(Test = "the Modified Jarque-Bera",
                       MJB_statistic,
                       p_value)
  return(results)
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






