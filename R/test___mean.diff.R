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
  install_packages(c("ggpubr", "ggplot2", "ggstatsplot", "dplyr", "rstatix"))






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
  is.var.equal = pretest$homoscedasticity$result.df$is.homoscedastic
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
        is.balanced
        test_result <- stats::oneway.test(sub___as.formula(y = response_var, x = group_var),
                                          data = df,
                                          var.equal = is.equal.var)

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



  if(is.normal){
    ### 🟧 Parametric =========================================================================



  }else{
    ### 🟧 Non-Parametric =========================================================================
    # Games-Howelll

    # Dunn procedure


  }

  method = test_result_df_2$method[1]

  if(test_result_df_2$significance[1]){
    ### 🟧 ANOVA ==============================================================

  }


  ## 🟧 Define the function ==============================================================
  # Post hoc 분석을 위한 함수 정의
  perform_posthoc <- function(test_result, df, response_var, group_var) {

    if (test_result$method == "Wilcoxon rank sum test" || test_result$method == "Mann-Whitney-Wilcoxon test") {
      rstatix::dunn_test()
      # 🟧 Mann-Whitney post hoc 처리: pairwise 윌콕슨 테스트
      posthoc_result <- df %>%
        pairwise_wilcox_test(response_var ~ group_var, p.adjust.method = "BH")

    } else if (test_result$method == "Kruskal-Wallis rank sum test") {

      # 🟧 Kruskal-Wallis post hoc 처리: pairwise 크루스칼-월리스 테스트
      posthoc_result <- df %>%
        pairwise_kruskal_test(response_var ~ group_var, p.adjust.method = "BH")

    } else if (test_result$method == "Friedman's rank sum test") {

      # 🟧 Friedman post hoc 처리: Nemenyi test
      posthoc_result <- friedman_posthoc_test(df, response_var, group_var, subject = "subject", p.adjust.method = "BH")

    } else if (test_result$method == "One-way analysis of means (not assuming equal variances)") {
      # ANOVA의 post hoc 처리: pairwise t-test (등분산 가정하지 않음)
      posthoc_result <- df %>%
        pairwise_t_test(response_var ~ group_var, p.adjust.method = "BH", pool.sd = FALSE)
    } else {
      # 기본 처리 (parametric paired or unpaired t-tests)
      posthoc_result <- df %>%
        pairwise_t_test(response_var ~ group_var, p.adjust.method = "BH", paired = is.paired)
    }
    return(posthoc_result)
  }



  # 함수 내에서 pairwise_t_test 함수 사용
  posthoc_result <- df %>%
    pairwise_t_test(sub___as.formula(y = response_var, x = group_var), p.adjust.method = "BH", paired =F)


  # 함수 내에서 pairwise_t_test 함수 사용
  posthoc_result <- df %>%
    pairwise_t_test(response_var ~ group_var, p.adjust.method = "BH", paired = is.paired)
  # 사용 예: Test 수행 후 Post Hoc 분석
  # 비모수 예시: Kruskal-Wallis test
  test_result <- kruskal.test(response_var ~ group_var, data = df)
  total_result <- list(test_result = test_result)
  total_result$combined_result <- data.frame(method = test_result$method, statistic = test_result$statistic, p.value = test_result$p.value)

  # Post Hoc 수행
  posthoc_results <- perform_posthoc(total_result$test_result, df, "response_var", "group_var")
  print(posthoc_results)


  posthoc.list = list()
  for(k in 1:nrow(adj_pvlas_signif)){
    if(adj_pvlas_signif[k]){


    }
  }






  ## 🟥 Combine data #############################################################################
  Combined_results.df = lapply(results.list, function(y){
    y$combined_result
  }) %>% do.call(rbind, .)

  Combined_results.df %>% View



  # 🟥CLT ##########################################################################
  is_large_sample  = as.numeric(Results.df$Norm_results$N_Obs) >= 30
  n_group = Results.df$Norm_results$N_Obs %>% length

  if(sum(is_large_sample) == n_group){

    Results.df$Norm_results$Norm_Tests = "The Central Limit Theorem"
    Results.df$Norm_results$Norm_p.val = "NA"
    Results.df$Norm_results$is.normal = "Asymptotic"
    Results.df$is.normal = TRUE
  }

  # 🟥 4) Combine results ===========================================================
  if(! 3 * length(Response_Vars) == length(Results.list$Normality) + length(Results.list$Homoscedasticity) + length(Results.list$ANOVA)){
    stop("There is a variable which is not done yet")
  }

  # Combine by each variable for all results
  Combined_Results.list = lapply(seq_along(Response_Vars), function(k){

    kth_Norm = Results.list$Normality[[k]]
    kth_Homo = Results.list$Homoscedasticity[[k]]
    kth_ANOVA = Results.list$ANOVA[[k]][[1]]


    kth_plots = list(Normality = kth_Norm$Norm_Plots, Boxplot = kth_ANOVA$Boxplot)
    kth_results = ccbind(X = kth_Norm$Norm_Test_Result$Norm_results, Y = kth_Homo) %>%
      ccbind(., kth_ANOVA$Result) %>%
      ccbind(df.frame(Response_Var = Response_Vars[k], Group_Var = Group_Var), .)

    kth_results_ReplaceNA = kth_results %>% dplyr::mutate_all(~ifelse(is.na(.), "", .))


    list(plots = kth_plots, results = kth_results, results_kable = kth_results_ReplaceNA)

  }) %>% setNames(Response_Vars)





  # 🟥 5) Export ANOVA Results as df frame ===========================================================
  # if(!is.null(path_save)){
  #   file.name = paste0("[ANOVA] Results_", "`", Group_Var, "`")
  #
  #   # save a combined df
  #   # Test___MeanDiff___Export.xlsx.Highlight(Combined_Results.df,
  #   #                                         path_save,
  #   #                                         file.name,
  #   #                                         group_var_type)
  #   cat("\n", crayon::green("Exporting"), crayon::red("Mean Difference Results"), crayon::green("is done!"),"\n")
  # }




  # 🟥 7) Return ===========================================================
  cat("\n", crayon::bgCyan("Analaysis is done!"),"\n")
  return(Combined_Results.list)
}



#==================================================================================
# 6) Save combined result
#==================================================================================
# if(!is.null(path_save)){
#   file.name = paste0("[ANOVA] Combined_Results_", "`", Group_Var, "`")
#
#   # save RDS file
#   saveRDS(object = Combined_Results.list, file = paste0(path_save, "/", file.name, ".rds"))
#
#   cat("\n", crayon::green("Exporting"), crayon::red("Combined Results"), crayon::green("is done!"),"\n")
# }


# # Combining tables for LaTeX
# Combined.list = lapply(list.files(path_save, pattern = "@_Combined Results for Latex Table", full.names=T), read.csv)
# First_Cols = Combined.list[[1]][,1:3]
# Second_Cols = lapply(Combined.list, function(x) x[,-c(1:3)])
# Combined_New.list = c(First_Cols, Second_Cols)
# Combined_New.df = do.call(cbind, Combined_New.list)
# Which_rows_to_highlight = apply(Combined_New.df, 1, function(x){
#   x = Combined_New.df[1,] %>% unlist
#   only_having_ns = sum(x[-c(1:3)] %in% c("none", "HNS", "NS")) == length(x)
#   return(!only_having_ns)
# }) %>% which
# Export___xlsx___Highlighting(Combined_New.df, colors.list = "red", which_cols.list = 1:ncol(Combined_New.df), coloring_index.list = Which_rows_to_highlight, path_save = path_save, file.name = "@@_Combined Results for Latex Table", sheet.name = "")
# write.csv(Combined_New.df, paste0(path_save, "/@@_Combined Results for Latex Table", ".csv"), row.names=F)
#




# Combining p.values
# Combined.list = lapply(list.files(path_save, pattern = "@_Only p.values Combined Results", full.names=T), read.csv)
# write.csv(do.call(rbind, Combined.list), paste0(path_save, "/@@_Only p.values Combined Results", ".csv"), row.names=F)








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
