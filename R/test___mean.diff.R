# data = df = iris
# group_var = "Species"
# response_vars = names(df) %>% grep("Species", ., value =T, invert = T)
# group_var_type = "nominal"
test___mean.diff = function(df,
                            group_var,
                            group_var_type = c("nominal", "ordinal"),
                            response_var,
                            is.paired.vec =FALSE,
                            alpha_anova = 0.05,
                            alpha_posthoc = 0.05,
                            p.adj.method_normality = "bonferroni",
                            post.hoc_method = c("none",
                                                "TukeyHSD",
                                                "Holm",
                                                "Hochberg",
                                                "SidakSS",
                                                "SidakSD",
                                                "BH",
                                                "BY",
                                                "ABH",
                                                "TSBH"),
                            p.adjust.method = c("none",
                                                "Bonferroni")
                            ...){
  # Significance level
  # alpha_anova = 0.05,
  # p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
  # type = c("parametric", "nonparametric", "robust", "bayes"),
  # Figure
  # label.as.p.val=F,
  # group.comparison=F,
  # lines.connecting.medians=F,
  # plot_title="",
  # plot_width = 10,
  # plot_height = 7.5,
  # plot_units = "in",
  # plot_dpi = 200,
  # results.subtitle=T,
  # exporting
  ## 🟥 packages ===================================================================
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?
    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }
  install_packages(c("ggpubr", "ggplot2", "conover.test", "ggstatsplot", "dplyr", "rstatix", "reshape2"))






  ## 🟥 Normality & Homoscedasticity ===========================================================
  pretest = test___homoscedasticity(df = df,
                                    group_var = group_var,
                                    response_var = response_var,
                                    p.adjust.method_normality = p.adj.method_normality,
                                    path_save = path_save)










  ## 🟥 Decision ==================================================================================
  ### 🟧 is normal ===============================================
  normality = pretest$normality$test_result
  if(sum(normality$n_obs >= 30) == nrow(normality)){
    # CLT
    is.normal = TRUE
  }else{
    is.normal = sum(normality$is.normal) == nrow(normality)
  }
  ### 🟧 is equal var ===============================================
  is.equal.var = pretest$homoscedasticity$result.df$is.homoscedastic
  ### 🟧 is pair ===============================================
  is.paired = is.paired
  ### 🟧 is balanced ===============================================
  is.balanced = df %>%
    group_by(!!sym(group_var)) %>%
    summarise(n_obs = n()) %>%
    pull(n_obs) %>%
    unique() %>%
    length() == 1
  ### 🟧 n_groups ===============================================
  n_groups <- df %>% pull(group_var) %>% unique() %>% length()
  ### 🟧 is skewed? ===============================================
  is.skewed = FALSE
  ### 🟧 is severely unequal ===============================================
  # 그룹별 관측값 수 계산
  group_observations <- df %>%
    group_by(!!sym(group_var)) %>%
    summarise(n_obs = n(), .groups = 'drop')
  # 최대 관측값과 최소 관측값의 차이 계산
  max_obs = max(group_observations$n_obs)
  min_obs = min(group_observations$n_obs)
  obs_difference = max_obs - min_obs
  # 관측값 차이가 10 이상인지 확인
  is.severely.unequal = obs_difference >= 10
  ### 🟧 is min 6=======================================================================
  is.min.6 = min_obs >= 6








  ## 🟥 Mean Difference Test ===========================================================
  ### 🟧 Nominal =====================================================================================
  if(group_var_type == "nominal"){
    if (is.normal) {
      #### 🟩 Parametric =====================================================================================
      if(n_groups == 2){
        ##### 🟦2groups: t-test ===================================================================
        stop("check")
        test_result = stats::t.test(formula = sub___as.formula(y = response_var, x= group_var),
                               data = df,
                               alternative = c("two.sided"),
                               mu = 0,
                               paired = is.paired,
                               var.equal = is.var.equal,
                               conf.level = 1-alpha_anova)
      } else {
        ##### 🟦3groups: ANOVA ===================================================================
        # oneway.test는 Welch의 ANOVA를 실행
        if(is.equal.var){
          ###### 🟪 equal var ==========================================================
          test_result = stats::aov(sub___as.formula(y = response_var, x = group_var), data = df)
        }else{
          ###### 🟪 unequal var ==========================================================
          test_result <- stats::oneway.test(sub___as.formula(y = response_var, x = group_var),
                                            data = df,
                                            var.equal = is.equal.var)
        }
      }
    } else {
      #### 🟩 Nonparametric =====================================================================================
      if (n_groups == 2) {
        ##### 🟦2groups =====================================================================================
        stop("nonpara 2 groups")
        test_result = stats::wilcox.test(sub___as.formula(y = response_var, x = group_var),
                                         data = df,
                                         paired = is.paired)

      } else {
        ##### 🟦3groups =====================================================================================
        if(is.paired){
          ###### 🟪paired:프리드먼 =====================================================================================
          stop("non para 3 g paired")
          test_result = stats::friedman.test(sub___as.formula(response_var, group_var), data = df)

        }else{
          ###### 🟪non-paired:크루스칼 =====================================================================================
          test_result = stats::kruskal.test(sub___as.formula(response_var, group_var), data = df)
        }# paired
      }# n_groups
    }# is normal
  }else{
    ### 🟧 Ordinal =====================================================================================
    stop("Ordinal?")
  }






  ## 🟥 Combine results ===========================================================
  ### 🟧 mean diff test =============================================================
  test_result_df <- tibble(
    stderr = test_result$stderr,
    method = test_result$method,
    statistic = test_result$statistic,
    parameter = ifelse(length(test_result$parameter) == 1, test_result$parameter, NA_real_),
    parameter_num_df = ifelse(length(test_result$parameter) == 2, test_result$parameter[1], NA_real_),
    parameter_denom_df = ifelse(length(test_result$parameter) == 2, test_result$parameter[2], NA_real_),
    p.value = test_result$p.value,
    conf_int_lower = test_result$conf.int[1],
    conf_int_upper = test_result$conf.int[2],
    null.value = test_result$null.value,
    alternative = test_result$alternative
  )


  ### 🟧 mean & median by group =============================================================
  summary_df <- df %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n_obs = n(), # 관측치 수
      sample_mean = mean(!!sym(response_var), na.rm = TRUE), # 평균
      sample_median = median(!!sym(response_var), na.rm = TRUE), # 중앙값
      .groups = 'drop' # 그룹화 해제
    )
  test_result_df_2 = data.frame(response = response_var) %>%
    cbind(data.frame(group = group_var)) %>%
    ccbind(summary_df) %>%
    ccbind(test_result_df) %>%
    ccbind(data.frame(alpha = alpha_anova))



  ### 🟧 significance =============================================================
  pval = test_result_df_2$p.value[1]
  test_result_df_2 = test_result_df_2 %>%
    ccbind(data.frame(significance = pval <= alpha_anova)) %>%
    ccbind(data.frame(significance_2 = sub___p.vals.signif.stars(pval)))





  ### 🟧 combine pretest =============================================================
  # combined.df = pretest$normality$test_result %>%
  #   ccbind(pretest$homoscedasticity$result.df) %>%
  #   ccbind(test_result_df)









  ## 🟥 Post-hoc ===========================================================================
  # 참고 논문: Comparing multiple comparisons - practical guidance for choosing the best multiple comparisons test
  # -> 아직 안 추가한 방법론들 있으므로 나중에 참고
  # 다른 분석을 할 때는 옵시디언 태그들 참조해서 다시 한 번 검토할 것
  if(is.normal){
    ### 🟧 Parametric + Unplanned comparisons ==============================================================================
    #### 🟨 pairwise t-test + p.val.adj =============================================================================================
    ##### 🟦 test =====================================================================================
    # t-test
    pairwise_results = pairwise.t.test(x = df[[response_var]],
                                        g = df[[group_var]],
                                        pool.sd = is.equal.var,
                                        paired = FALSE,
                                        p.adjust.method = "none")  # 일단 보정하지 않고 원래의 p-value를 얻습니다.
    # 데이터 프레임 생성
    pairwise_df <- as.data.frame(pairwise_results$p.value)

    # 데이터 프레임의 row names을 첫 번째 열로 변환
    pairwise_df$Group1 <- row.names(pairwise_df)

    # 긴 형식으로 데이터 프레임 변환
    long_pairwise_df <- melt(pairwise_df, id.vars = "Group1", variable.name = "Group2", value.name = "p.value")

    # NA 값을 포함하는 행 제거
    long_pairwise_df <- na.omit(long_pairwise_df)

    # add col
    long_pairwise_df = long_pairwise_df %>%
      mutate(pairwise_comparison = ifelse(is.equal.var, "t-test", "Welch's t-test")) %>%
      relocate(pairwise_comparison)





    ##### 🟦 Adjust p-values =====================================================================================
    pairwise_results.list = list()
    # t-test
    pairwise_results.list[["pairwise-t.test"]] = long_pairwise_df %>%
      mutate(significance = sub___p.vals.signif.stars(p.value))
    # Bonferroni
    pairwise_results.list[["Bonferroni"]] <- long_pairwise_df %>%
      cbind(sub___p.adjust(long_pairwise_df$p.value, method = "bonferroni", only.return.p.vals = F)) %>%
      dplyr::select(-p.value) %>%
      mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Bonferroni)", "Pairwise Welch's t-test (Bonferroni)"))
    # Holm
    pairwise_results.list[["Holm"]] <- long_pairwise_df %>%
      cbind(sub___p.adjust(long_pairwise_df$p.value, method = "holm", only.return.p.vals = F)) %>%
      dplyr::select(-p.value) %>%
      mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Holm)", "Pairwise Welch's t-test (Holm)"))
    # Dunn-Sidak
    pairwise_results.list[["Dunn-Sidak"]] <- long_pairwise_df %>%
      cbind(sub___p.adjust(p.values = long_pairwise_df$p.value, method = "SidakSS", only.return.p.vals = F)) %>%
      dplyr::select(-p.value) %>%
      mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Dunn-Sidak)", "Pairwise Welch's t-test (Dunn-Sidak)"))




    if(is.equal.var){
      #### 🟨 equal var =============================================================================================
      ##### 🟩 !severely unequal =============================================================================================
      if(!is.severely.unequal){
        ###### 🟦 TukeyHSD =============================================================================================
        pairwise_results.list[["TukeyHSD"]] <- TukeyHSD(test_result)
      }


    }else{
      #### 🟨 not equal var =============================================================================================
      if(is.min.6){
        ##### 🟩 more than 6 obs? =============================================================================================
        ###### 🟦 Games-Howell =============================================================================================
        pairwise_results.list[["Games-Howell"]] = df %>%
          rstatix::games_howell_test(sub___as.formula(response_var, group_var)) %>%
          mutate(post.hoc_method = "Games-Howell") %>%
          relocate(post.hoc_method)

        pairwise_results.list[["Games-Howell"]]$p.adj.signif = sub___p.vals.signif.stars(pairwise_results.list[["Games-Howell"]]$p.adj)
      }else{
        ##### 🟩 less than 6 obs =============================================================================================
        stop("less than 6 obs")
      }
    }
  }else{
    ### 🟧 Non-Parametric =========================================================================
    ##### 🟩 Dunn Procedure =============================================================================================
    pairwise_results.list[["Dunn-test"]] = df %>%
      rstatix::dunn_test(sub___as.formula(response_var, group_var), p.adjust.method = "none") %>%
      mutate(post.hoc_method = "Dunn test") %>%
      relocate(post.hoc_method) %>%
      mutate(p.adj.signif = sub___p.vals.signif.stars(p.adj))


    ##### 🟩 Conover-Iman-test =============================================================================================
    # Dunn test보다 높은 검정력
    # Kruskal-Wallis 검정이 유의한 경우만 유의
    conover = conover.test::conover.test(x = df[[response_var]], g = df[[group_var]])
    pairwise_results.list[["Conover-Iman test"]] = data.frame(post.hoc_method = "Conover-Iman test",
               comparisons = conover$comparisons,
               t.statistics = conover$`T`, # a vector of all _m_ of the Conover-Iman _t_ test statistics.
               p.adjusted = conover$P.adjusted) %>%
      ccbind(data.frame(chi2 = conover$chi2)) %>%  # a scalar of the Kruskal-Wallis test statistic adjusted for ties.,
      mutate(significance = sub___p.vals.signif.stars(p.adjusted)) %>%
      relocate(significance, .after = p.adjusted)
  }







  ## 🟥 Select Post-hoc by recommendation ===========================================================================






  ## 🟥 Boxplots ===========================================================================









  # 🟥 7) Return ===========================================================
  cat("\n", crayon::bgCyan("Analaysis is done!"),"\n")
  return(Combined_Results.list)
}
#==================================================================================
# 5) Boxplot
#==================================================================================
# Boxplot.list = lapply(Results_ANOVA, function(ith_Results){
#   Test___MeanDiff___Single.Responses___Box.Plot(df,
#                                                 ith_Results,
#                                                 label.as.p.val,
#                                                 group.comparison,
#                                                 lines.connecting.medians,
#                                                 path_save)
# })
