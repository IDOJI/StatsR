Test_Correlation = function(df, y, x=NULL, p.adj.method = "bonferroni") {
  # 🟥Arguments ###################################################################################
  ## 🟨col_names ================================================================================
  if(!is.null(col_names)){
    df = df[, col_names]
  }



  # 🟥Method ###################################################################################
  # n >= 30 => limiting distribution
  if











  # 🟥Results List ################################################################################
  Results.list = list()






  # 🟥Correlation Tests #########################################################################
  # corr tests
  Results.list$Correlation_Test = lapply(x, function(ith_x){

    ith_result = cor.test(df[,ith_x], df[,y], method = method) %>%
      unlist() %>%
      as.data.frame() %>%
      t()


    View(ith_result)
    ith_result %>% unlist
    ith_result$statistic
    ith_result$parameter
    ith_result$p.value
    ith_result$estimate

  }) %>% setNames(x)



  # adjust p values



  # 🟥Visualization #########################################################################
  ## 🟨Corrleation plot ============================================================================
  Results.list$Plot$Total = ggplot___correlation(df, y, x, method, p.adj.method)



  ## 🟨Scatter plot ============================================================================
  Results.list$Plot$Scatter = lapply(x, function(ith_x){
    ggplot___scatterplot(df, ith_x, y, method)
  }) %>% setNames(x)



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

# 함수 사용 예시
# 예를 들어, data는 데이터 프레임이고, 'column1'과 'column2'는 분석하고자 하는 두 열의 이름입니다.
result <- calculate_correlation(data, 'column1', 'column2')
print(result)





Test___Corr = function(Data,
                       x_Vars = NULL,
                       y_Vars = NULL,
                       alpha=0.05,
                       x_lab = "",
                       y_lab = "",
                       # cor.vars = NULL,
                       # cor.vars.names = NULL,
                       # matrix.type = "upper",
                       type = "parametric",
                       # tr = 0.2,
                       # partial = FALSE,
                       # k = 2L,
                       # sig.level = 0.05,
                       # conf.level = 0.95,
                       # bf.prior = 0.707,
                       p.adjust.method = "TSBH",
                       # pch = "cross",
                       # ggcorrplot.args = list(method = "square",
                       #                        outline.color = "black",
                       #                        pch.cex = 14),
                       # package = "RColorBrewer",
                       # palette = "Dark2",
                       colors = c("#E69F00", "white", "#009E73"),
                       # ggtheme = ggstatsplot::theme_ggstatsplot(),
                       # ggplot.component = NULL,
                       # title = NULL,
                       # subtitle = NULL,
                       # caption = NULL,
                       save.path = NULL,
                       ...)
{
  # colors = c("#E69F00", "white", "#009E73")
  # colors = c("blue", "white", "red")
  #=============================================================================
  # save.path
  #=============================================================================
  if(!is.null(save.path)){
    dir.create(save.path, F)
  }
?varmx.pca.fd






  #=============================================================================
  # Correlation  & plotting
  #=============================================================================
  # partial
  if(!is.null(x_Vars) && !is.null(y_Vars)){
    Corr.df = Test___Corr___Partial(Data, x_Vars, y_Vars, type, p.adjust.method, alpha, x_lab, y_lab, colors, save.path)
  }else{
    # Full
    Corr.df = Test___Corr___Full(Data)
  }







  #=============================================================================
  # Return
  #=============================================================================
  return(Corr.df)
}






















