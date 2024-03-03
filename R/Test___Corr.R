Test_Correlation = function(df, y, x=NULL, alpha = 0.05, method = "pearson", outlier_method="IQR", p.adj.method = "bonferroni") {
  # 🟥Arguments ###################################################################################
  ## 🟨col_names ================================================================================
  if(!is.null(col_names)){
    df = df[, col_names]
  }


  # 🟥Method ###################################################################################
  # Checking normality for Pearson correlation
  N_obs = nrow(df)

  # x normal?
  if(!is.null(x)){
    x_Norm = sapply(x, function(ith_x){
      Test___Normality___Single.Vector(df[,ith_x], outlier_method, 0.05)$is.Normal
    })

    if(sum(x_Norm) != length(x)){
      x_Norm = FALSE
    }
  }

  # y normal?
  y_Norm = Test___Normality___Single.Vector(df[,y], outlier_method, 0.05)
  y_Norm = y_Norm$is.Normal


  # n >= 30 => limiting distribution of MME (=MLE)
  is_Limiting_Distribution_of_MME = N_obs >= 30
  # joint pdf of x and y = Bivariate Normal => sampling distribution
  is_Joint_Normal = x_Norm && y_Norm

  if(!(is_Limiting_Distribution_of_MME || !is_Joint_Normal) && method == "pearson"){

    warning("The sample size is small or These variables are not Normal distributed. Check if using Pearson is correct.")

  }







  # 🟥Results List ################################################################################
  Results.list = list()



  # 🟥Correlation Tests #########################################################################
  # corr tests
  Results.list$Correlation_Test = lapply(x, function(ith_x){

    ith_result = cor.test(df[,ith_x], df[,y], method = method) %>%
      unlist() %>%
      as.data.frame() %>%
      t()

    return(ith_result)

  }) %>% do.call(rbind, .) %>% as.data.frame()


  # Add variables names
  Results.list$Correlation_Test$Variables = x

  # adjust pvals
  Adjust_P.vals = SUB___P.vals.Adjust(raw_p.vals = Results.list$Correlation_Test$p.value, method = p.adj.method, alpha)
  Results.list$Correlation_Test = cbind(Results.list$Correlation_Test, Adjust_P.vals)

  # remove rownames
  rownames(Results.list$Correlation_Test) = NULL


  # move cols
  Results.list$Correlation_Test = Results.list$Correlation_Test %>%
    dplyr::relocate("Variables") %>%
    dplyr::select(-c("data.name"))



  # 🟥Visualization #########################################################################
  ## 🟨Corrleation plot ============================================================================
  Results.list$Plot$Total = ggplot___correlation(df, y, x, method, p.adj.method)



  ## 🟨Scatter plot ============================================================================
  Results.list$Plot$Scatter = lapply(x, function(ith_x){
    ggplot___scatterplot(df, ith_x, y, method)
  }) %>% setNames(x)

  Results.list$Plot$Scatter$hp

  # 🟥Arguments ###############################################################
  # 연속성 및 정규성 검사
  if (is.numeric(x) && is.numeric(y) && shapiro.test(x)$p.value > 0.05 && shapiro.test(y)$p.value > 0.05) {
    # 두 변수 모두 정규 분포를 따르는 경우 Pearson 상관계수 사용
    test_result <- cor.test(x, y, method = "pearson")
  } else {
    # 그 외의 경우 Spearman 상관계수 사용
    test_result <- cor.test(x, y, method = "spearman")
  }

  # 결과 반환
  return(list(correlation_coefficient = test_result$estimate,
              p_value = test_result$p.value,
              method = test_result$method))
}




















