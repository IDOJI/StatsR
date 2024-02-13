Test___Equal.Var = function(Data,
                            Group_Var,
                            Response_Vars,
                            is.Normal,
                            outlier_method,
                            alpha=0.05){
  if(length(Response_Vars) != length(is.Normal)){
    stop("The length of Response_Vars and is.Normal is different!")
  }



  Results.list = lapply(seq_along(Response_Vars), function(i){
    ith_Response_Var = Response_Vars[i]
    ith_is.Normal = is.Normal[i]

    ### Normality : TRUE
    if(ith_is.Normal){
      ith_Results = Test___Equal.Var___When.Norm.True(Data,
                                                      Group_Var,
                                                      Response_Var = ith_Response_Var,
                                                      is.normal = ith_is.Normal,
                                                      outlier_method = outlier_method,
                                                      alpha)
    }else{
    ### Normality : FALSE
      ith_Results = Test___Equal.Var___When.Norm.False(Data,
                                         Group_Var,
                                         Response_Var = ith_Response_Var,
                                         is.normal = ith_is.Normal,
                                         outlier_method = outlier_method,
                                         alpha)
    }

    ith_Results = data.frame(Homoscedasticity_Test = ith_Results$Equal.Var_What.Test,
                             Homoscedasticity_p.val = ith_Results$Equal.Var_p.val,
                             is.Homoscedastic = ith_Results$is.Equal.Var)

    return(ith_Results)
  })


  names(Results.list) = Response_Vars


  cat("\n", crayon::green("Testing") ,crayon::red("Homogeneity"), crayon::green("is done!"),"\n")
  return(Results.list)
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
