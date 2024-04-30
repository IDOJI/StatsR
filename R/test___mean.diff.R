# data = df = iris
# group_var = "Species"
# response_vars = names(df) %>% grep("Species", ., value =T, invert = T)
# group_var_type = "nominal"
test___mean.diff = function(df,
                            group_var,
                            group_var_type = c("nominal", "ordinal"),
                            response_var,
                            is.paired =FALSE,
                            alpha_anova = 0.05,
                            alpha_posthoc = 0.05,
                            p.adj.method_normality = "bonferroni",
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
        test_result = stats::t.test(formula = sub___as.formula(y = response_var, x= group_var),
                                    data = df,
                                    alternative = c("two.sided"),
                                    mu = 0,
                                    paired = is.paired,
                                    var.equal = is.equal.var,
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
    post.hoc_results.list = list()
    # t-test
    # post.hoc_results.list[["pairwise-t.test"]] = long_pairwise_df %>%
    #   mutate(p.adj = p.value) %>%
    #   mutate(p.adj.method = "none") %>%
    #   mutate(p.adj.signif = sub___p.vals.signif.stars(p.value))


    # Bonferroni: 20개 이상 그룹이면 너무 보수적
    if(n_groups < 20){
      post.hoc_results.list[["Bonferroni"]] <- long_pairwise_df %>%
        cbind(sub___p.adjust(p.values = long_pairwise_df$p.value, method = "bonferroni", only.return.p.vals = F)) %>%
        dplyr::select(-p.value) %>%
        mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Bonferroni)", "Pairwise Welch's t-test (Bonferroni)")) %>%
        rename(group1 := Group1) %>%
        rename(group2 := Group2)
    }
    # Holm
    post.hoc_results.list[["Holm"]] <- long_pairwise_df %>%
      cbind(sub___p.adjust(long_pairwise_df$p.value, method = "holm", only.return.p.vals = F)) %>%
      dplyr::select(-p.value) %>%
      mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Holm)", "Pairwise Welch's t-test (Holm)")) %>%
      rename(group1 := Group1) %>%
      rename(group2 := Group2)
    # Dunn-Sidak
    post.hoc_results.list[["Dunn-Sidak"]] <- long_pairwise_df %>%
      cbind(sub___p.adjust(p.values = long_pairwise_df$p.value, method = "SidakSS", only.return.p.vals = F)) %>%
      dplyr::select(-p.value) %>%
      mutate(post.hoc_method = ifelse(is.equal.var, "Pairwise t-test (Dunn-Sidak)", "Pairwise Welch's t-test (Dunn-Sidak)")) %>%
      rename(group1 := Group1) %>%
      rename(group2 := Group2)




    if(is.equal.var){
      #### 🟨 equal var =============================================================================================
      ##### 🟩 !severely unequal =============================================================================================
      if(!is.severely.unequal){
        ###### 🟦 TukeyHSD =============================================================================================
        # TukeyHSD 결과
        post.hoc_results.list[["Tukey HSD"]] <- TukeyHSD(test_result)[[1]] %>%
          as.data.frame() %>%
          rownames_to_column(var = "comparison") %>%
          mutate(
            group1 = sapply(strsplit(comparison, "-"), `[`, 1),
            group2 = sapply(strsplit(comparison, "-"), `[`, 2)
          ) %>%
          dplyr::select(-comparison) %>%   # comparison 열 제거
          mutate(post.hoc_method = "Tukey HSD") %>%
          relocate(post.hoc_method) %>%
          relocate(group2) %>%
          relocate(group1) %>%
          mutate(p.adj.signif = sub___p.vals.signif.stars(p.adj))
      }
    }else{
      #### 🟨 not equal var =============================================================================================
      if(is.min.6){
        ##### 🟩 more than 6 obs? =============================================================================================
        ###### 🟦 Games-Howell =============================================================================================
        post.hoc_results.list[["Games-Howell"]] = df %>%
          rstatix::games_howell_test(sub___as.formula(response_var, group_var)) %>%
          mutate(post.hoc_method = "Games-Howell test") %>%
          relocate(post.hoc_method) %>%
          mutate(p.adj.signif = sub___p.vals.signif.stars(p.adj))
          # rename(Group1:=group1) %>%
          # rename(Group2:=group2)


      }else{
        ##### 🟩 less than 6 obs =============================================================================================
        stop("less than 6 obs")
      }
    }
  }else{
    ### 🟧 Non-Parametric =========================================================================
    ##### 🟩 Dunn Procedure =============================================================================================
    post.hoc_results.list[["Dunn-test"]] = df %>%
      rstatix::dunn_test(sub___as.formula(response_var, group_var), p.adjust.method = "none") %>%
      mutate(post.hoc_method = "Dunn test") %>%
      relocate(post.hoc_method) %>%
      mutate(p.adj.signif = sub___p.vals.signif.stars(p.adj))


    ##### 🟩 Conover-Iman-test =============================================================================================
    # Dunn test보다 높은 검정력
    # Kruskal-Wallis 검정이 유의한 경우만 유의
    conover = conover.test::conover.test(x = df[[response_var]], g = df[[group_var]])
    post.hoc_results.list[["Conover-Iman test"]] = data.frame(post.hoc_method = "Conover-Iman test",
               comparisons = conover$comparisons,
               t.statistics = conover$`T`, # a vector of all _m_ of the Conover-Iman _t_ test statistics.
               p.adj = conover$P.adjusted) %>%
      ccbind(data.frame(chi2 = conover$chi2)) %>%  # a scalar of the Kruskal-Wallis test statistic adjusted for ties.,
      mutate(p.adj.signif = sub___p.vals.signif.stars(p.adj)) %>%
      relocate(significance, .after = p.adjusted)
  }




  ## 🟥 Select Post-hoc by recommendation ===========================================================================
  # the smallest p-values
  summed_p_vals = sapply(post.hoc_results.list, function(y){
    y[["p.adj"]] %>% sum
  })
  selected_post.hoc = post.hoc_results.list[[which.min(summed_p_vals)]]






  ## 🟥 Boxplots ===========================================================================
  p = ggplot___boxplot___mean.diff.test(df,
                                        response_var,
                                        group_var,
                                        test_result.df = test_result_df_2,
                                        post.hoc_result = selected_post.hoc,
                                        path_save = path_save)






  ## 🟥 combine results ===========================================================================
  final.list = list()
  final.list[["pretest"]] = pretest
  final.list[["test result"]] = test_result
  final.list[["test result as data.frame"]] = test_result_df_2
  if(test_result_df_2$significance[1]){
    final.list[["post hoc with the smallest adj p-values"]] = selected_post.hoc
  }
  final.list[["box plots"]] = p


  # 🟥 7) Return ===========================================================
  cat("\n", crayon::bgCyan("Analaysis is done!"),"\n")
  return(final.list)
}


# ⭐️ ggplot___boxplot___mean.diff.test ==============================================================
ggplot___boxplot___mean.diff.test = function(df,
                                             response_var,
                                             group_var,
                                             test_result.df,
                                             post.hoc_result,
                                             add.violin = TRUE,
                                             connect.medians = FALSE,
                                             add.group.comparison = TRUE,
                                             path_save = NULL){
  # 🟥 install.package ==============================================================================
  install_packages(c("EnvStats", "ggpubr", "gridExtra", "grid")) %>% invisible




  # 🟥 pallette =============================================================================
  install_packages("RColorBrewer")
  # Step 1: Generate palette
  all_colors <- brewer.pal(12, "Set3")  # 12 is the maximum for Set3

  # Step 2: Filter out undesired color
  filtered_colors <- all_colors[all_colors != "#FFFFB3"]

  # Step 3: Check if you need more colors
  n_colors <- df[[group_var]] %>% unlist %>% unique %>% length

  if (length(filtered_colors) < n_colors) {
    # This is just an example: you might want to add a color or generate colors in another way
    filtered_colors <- c(filtered_colors, "#FF0000")
  }

  # Use 'filtered_colors' in your plot
  colors <- filtered_colors[1:n_colors]






  # 🟥 p1 : Boxplot =============================================================================
  p1 <- ggpubr::ggboxplot(data = df,
                          x = group_var,
                          y = response_var,
                          color = group_var,
                          palette = colors,
                          # shape = Group_Var,
                          size = 1,
                          add = "jitter",
                          add.params = list(size=1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold")) +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 10)) # change text size of theme components
  # Label angle
  # p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  # Label bold
  # p2 = p1 + ggpubr::font("xlab", size = 20, face = "bold") + ggpubr::font("ylab", size = 20, face = "bold")








  # 🟥 p2 : violin =============================================================================
  if(add.violin){
    p2 = p1 + geom_violin(adjust = 0.4, fill = NA)
  }else{
    p2 = p1
  }







  # 🟥 p3 : pointing out mean ==============================================================================================
  p3 = p2 + stat_summary(fun = mean, geom = "point", shape = 16, size = 4, color = "maroon")







  # 🟥 p4 : adding lines connecting neighboing medians ===================================================================
  if(connect.medians){

    p4 = p3 + stat_summary(fun = median, geom = "line", group = 1, aes(group = 1), color = "blue")

  }else{

    p4 = p3

  }






  # 🟥 p5 : Adding methods =============================================================================
  if(nrow(test_result.df)>2){
    p5 = p4 + ggtitle(paste0(test_result.df$method[1], "\n", post.hoc_result$post.hoc_method[1]))
  }else{
    p5 = p4 + ggtitle(paste0(test_result.df$method[1]))
  }







  # 🟥 p6 : Adding p-values on comparing groups =============================================================================
  # 두 그룹 사이의 비교 옵션 넣기
  if(add.group.comparison){
    # 필터링된 데이터에서 유의미한 결과만 사용
    significant_results <- post.hoc_result %>%
      dplyr::filter(p.adj <= 0.05)


    # ggpubr::stat_pvalue_manual을 사용하여 박스플롯에 유의성 표시 추가
    p6 <- p5 + ggpubr::stat_pvalue_manual(
      data = significant_results,
      label = "p.adj.signif",  # 이 열이 별표("***", "**", "*") 유의성 표시를 포함하고 있다고 가정
      y.position = 1.1 * max(df[[response_var]], na.rm = TRUE),  # 유의성 표시 위치
      step.increase = 0.1,  # 선의 높이 조절
      vjust = -0.5  # 세로 위치 조정
    )
  }else{

    p6 = p5

  }





  # 🟥 p7 : Adding sample size =============================================================
  p7 = p6 + EnvStats::stat_n_text(text.box = F, size = 4)






  # 🟥 Export =============================================================================
  if(!is.null(path_save)){
    ggsave(filename = paste0(path_save, "/[Boxplot] ", group_var, " vs ", response_var, ".png"), plot = p5, device = "png", dpi = 300)
  }


  return(p7)
}






