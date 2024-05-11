# 🟥 Homoscedasticity ============================================================
test___homoscedasticity = function(df,
                                   group_var,
                                   response_var,
                                   alpha=0.05,
                                   outlier_method = "IQR",
                                   p.adjust.method_normality = "bonferroni",
                                   path_save){
  ## 🟧 Exclude NA =================================================================================
  # NA가 있는 행 제거
  which_na = which(is.na(df[[group_var]]) | is.na(df[[response_var]]))
  if(length(which_na)>0){
    df_NA = df[which_na, ]
    df = df[-which_na, ]
  }else{
    df_NA = NULL
  }



  ## 🟧 Normality ======================================================================================
  # Results
  normality.list = test___normality(df = df,
                                    group_var = group_var,
                                    response_var = response_var,
                                    outlier_method = outlier_method,
                                    p.adjust.method = p.adjust.method_normality,
                                    path_save = path_save)
  # is normal?
  is.normal = sum(normality.list$test_result$is.normal) == nrow(normality.list$test_result)





  ## 🟧 Homoscedasticity ======================================================================================
  if(is.normal){
    ### Normality : TRUE
    result = test___homoscedasticity___when.norm.true(df,
                                                      group_var,
                                                      response_var = response_var,
                                                      is.normal = is.normal,
                                                      outlier_method = outlier_method,
                                                      alpha)
  }else{
    ### Normality : FALSE
    result = test___homoscedasticity___when.norm.false(df,
                                                       group_var,
                                                       response_var = response_var,
                                                       is.normal = is.normal,
                                                       outlier_method = outlier_method,
                                                       alpha)
  }
  # data frame
  result.df = data.frame(homoscedasticity_test = result$homoscedasticity_what.test,
                         homoscedasticity_p.val = result$homoscedasticity_p.value,
                         is.homoscedastic = result$is.equal.variance)
  # combine as list
  result.list = list(test = result$homoscedasticity_result,
                     result.df = result.df)




  ## 🟧 Combine data ======================================================================================
  combined.list = list(normality = normality.list,
                       homoscedasticity = result.list,
                       NA_rows = df_NA)






  cat("\n", crayon::green("Testing") ,crayon::red("Homogeneity"), crayon::green("is done!"),"\n")
  return(combined.list)
}


# 🟥 when.norm.False ============================================================
test___homoscedasticity___when.norm.false = function(df,
                                                     group_var,
                                                     response_var,
                                                     is.normal,
                                                     outlier_method = c("IQR"),
                                                     alpha){
  # Check outliers
  Have_Outliers = tapply(
    df %>% dplyr::select(!!response_var) %>% unlist(),
    df %>% dplyr::select(!!group_var) %>% unlist(),
    function(x){
      ith_Outliers = sub___which.outliers(x, method = outlier_method)
      ith_Have_Outliers = length(ith_Outliers)>0
      return(ith_Have_Outliers)
    }
  ) %>% sum %>% as.logical



  # results
  results.list = list(NA, NA, NA, NA) %>% setNames(c("homoscedasticity_result",
                                                     "homoscedasticity_what.test",
                                                     "homoscedasticity_p.value",
                                                     "is.equal.variance"))


  # test
  if(Have_Outliers){
    results.list[[1]] = fligner.test(df[[response_var]], df[[group_var]])
    results.list[[2]] = "Fligner-Killeen"
    results.list[[3]] = results.list[[1]]$p.value %>% as.numeric
  }else{

    results.list[[1]] =car::leveneTest(y=df[,response_var] %>% unlist,  group=df[,group_var] %>% unlist) %>% suppressWarnings()
    results.list[[2]] = "Levene"
    results.list[[3]] = results.list[[1]]$`Pr(>F)`[1] %>% as.numeric
  }

  results.list[[4]] = results.list[[3]] > alpha # Test results : is it homoscedasticity?


  return(results.list)
}




# 🟥 when.norm.true ============================================================
test___homoscedasticity___when.norm.true = function(df, group_var, response_var, is.normal, outlier_method, alpha){
  data.list = as_list_by(df, group_var)

  results.list = list(NA, NA, NA, NA) %>% setNames(c("homoscedasticity_result",
                                                     "homoscedasticity_what.test",
                                                     "homoscedasticity_p.value",
                                                     "is.equal.variance"))

  if(length(data.list)==2){
    results.list[[1]] = var.test(data.list[[1]][,response_var] %>% unlist,
                                 data.list[[2]][,response_var] %>% unlist,
                                 conf.level = 1-alpha)
    results.list[[2]] = "F.test"
    results.list[[3]] = results.list[[1]]$p.value
  }else if(length(data.list)>2){
    results.list[[1]] = bartlett.test(sub___as.formula(response_var, group_var), df)
    results.list[[2]] = "Bartlett"
    results.list[[3]] = results.list[[1]]$p.value
  }

  results.list[[4]] = results.list[[3]] > alpha

  return(results.list)
}






# Paper : An Adjustment to the Bartlett's Test for Small Sample Size
# In conclusion,
# the adjustment has good control on the type I error and higher power,
# and thus is recommended for small samples and large population number
# when underlying distribution is normal.


# 3) 레빈
# 등분산 검정중 하나인 Levene 검정은
# 집단간 분산이 같은지 다른지 여부를 알아볼 때 사용하기도 하고
# 독립 2표본 t-검정 또는 일원분산분석(one-way ANOVA) 실시 전에
# 가정 때문에 확인하는 용도로 사용하기도 한다.
# 그리고 Levene 검정은 두 집단 뿐만 아니라
# 세 집단 이상에서도 사용할 수 있으며
# Bartlett 검정과 달리 표본이 정규성을 보이지 않아도 사용할 수 있다.


# 4) 비모수
# "Fligner-Killeen Test of Homogeneity of Variances,
# Performs a Fligner-Killeen (median) test of the null
# that the variances in each of the groups (samples) are the same"
