test___mean.diff___multiple = function(df,
                                       group_vars,
                                       group_var_type = "nomial",
                                       response_vars,
                                       is.paired = FALSE,
                                       alpha_anova = 0.05,
                                       alpha_posthoc = 0.05,
                                       path_save = NULL,
                                       combine_by = c("group_vars", "response_vars")){
  ## 🟧 mean diff test by "combine_by"  =========================================================
  if(combine_by == "group_vars"){

    results.list = list()

    for(j in seq_along(response_vars)){

      jth_results.list = list()

      for(i in seq_along(group_vars)){

        jth_results.list[[i]] = test___mean.diff(df = df,
                                                 group_var = group_vars[i],
                                                 group_var_type = group_var_type,
                                                 response_var = response_vars[j],
                                                 is.paired = is.paired,
                                                 alpha_anova = alpha_anova,
                                                 alpha_posthoc = alpha_posthoc,
                                                 path_save = path_save)

      }

      names(jth_results.list) = group_vars

      results.list[[j]] = jth_results.list

    }
    names(results.list) = response_vars

  }else if(combine_by == "response_vars"){
    results.list = rep(NA, times = length(group_vars)) %>% as.list
    for(i in seq_along(group_vars)){
      ith_results.list = list()
      for(j in seq_along(response_vars)){
        ith_results.list[[j]] = test___mean.diff(df = df,
                                                 group_var = group_vars[i],
                                                 group_var_type = group_var_type,
                                                 response_var = response_vars[j],
                                                 is.paired = is.paired,
                                                 alpha_anova = alpha_anova,
                                                 alpha_posthoc = alpha_posthoc,
                                                 path_save = path_save)
      }
      names(ith_results.list) = response_vars
      results.list[[i]] = ith_results.list
    }
    names(results.list) = group_vars
  }else{
    stop("the 'combine_by' should be either 'group_vars' or 'response_vars'!")
  }






  ## 🟧 Combine results =========================================================
  combined_results.list = list()
  results_old.list = results.list
  for(i in seq_along(results.list)){
    ith_results.list = results.list[[i]]
    # ith_results.list에서 각 요소를 추출하여 하나의 데이터 프레임으로 결합
    all_results.df <- dplyr::bind_rows(lapply(ith_results.list,
                                              function(x) x$`test result as data.frame`), .id = "group_var")
    selected_cols = c("group_var", "response", "method", "p.value", "alternative", "alpha", "significance", "significance_2")
    selected_results.df = all_results.df %>%
      dplyr::select(all_of(selected_cols)) %>%
      dplyr::filter(!is.na(response))

    # adjust p-values
    # 가장 작은 p값의 합을 제공하는 방법을 선택
    p.values = selected_results.df$p.value
    correction_methods = c("bonferroni", "holm", "SidakSS")
    names(correction_methods) = c("Bonferroni Correction", "Holm-Bonferroni", "Dunn-Sidak Correction")
    which_min_adj_p_values = sapply(correction_methods, function(y){
      adj_p_values = sub___p.adjust(p.values, method = y, alpha = 0.05)
      sum(adj_p_values)
    }) %>% which.min
    selected_results.df$adj.p.values = sub___p.adjust(p.values, method = correction_methods[which_min_adj_p_values], alpha = 0.05)
    selected_results.df$adj_method = names(correction_methods)[which_min_adj_p_values]


    ### 🟨 Signifiance =====================================================================
    selected_results.df$adj_significance = sub___p.vals.signif.stars(p.vals = selected_results.df$adj.p.values)


    ### 🟨 Final results =====================================================================
    combined_results.list[[i]] = selected_results.df


    ### 🟨 각 x의 데이터 프레임을 업데이트 =====================================================
    ith_results_new.list <- lapply(ith_results.list, function(x) {
      # x = ith_results.list[[1]]
      original_df <- x$`test result as data.frame`
      group_var_value <- original_df$group[1] # 가정: 모든 행에 동일한 group_var 값이 있다고 가정

      # all_results.df에서 해당 group_var 값에 해당하는 행만 추출
      updated_df <- selected_results.df %>% dplyr::filter(group_var  == group_var_value)


      # 두 데이터프레임을 합치는 코드
      intersect_names = intersect(names(original_df), names(updated_df))
      combined_df <- left_join(original_df, updated_df, by = c(intersect_names))

      combined_df$group_var = NULL

      # 업데이트된 데이터 프레임을 x에 다시 할당
      x$`test result as data.frame` <- combined_df

      # n_group==2이면 posthoc 수정
      x$`post hoc with the smallest adj p-values`$p.adj = x$`test result as data.frame`$adj.p.values[1]
      x$`post hoc with the smallest adj p-values`$p.adj.signif = sub___p.vals.signif.stars(x$`test result as data.frame`$adj.p.values[1])

      return(x)
    }) %>% setNames(names(ith_results.list))

    results.list[[i]] = ith_results_new.list
  }
  names(combined_results.list) = names(results.list)





  ## 🟧 modify plots  ==============================================================================================
  ### 🟩 특정 조건을 만족하는 레이어를 제거하는 함수 ===============================================================
  remove_layer_if_exists <- function(plot, geom_name, stat_name) {
    # 레이어 인덱스 목록을 초기화
    layer_indices_to_remove <- c()

    # 각 레이어를 검사하여 조건에 맞는 레이어 인덱스를 수집
    for (i in seq_along(plot$layers)) {
      layer <- plot$layers[[i]]
      if (inherits(layer$geom, geom_name) && inherits(layer$stat, stat_name)) {
        layer_indices_to_remove <- c(layer_indices_to_remove, i)
      }
    }

    # 조건에 맞는 레이어가 있으면 해당 레이어 제거
    if (length(layer_indices_to_remove) > 0) {
      plot$layers <- plot$layers[-layer_indices_to_remove]
    }

    return(plot)
  }




  ### 🟩 루프를 통해 결과 리스트 처리 =============================================================================
  for(j in seq_along(results_old.list)){
    jth_results_old <- results_old.list[[j]]
    jth_results_new <- results.list[[j]]

    for (k in seq_along(jth_results_old)){
      kth_variable_results_old <- jth_results_old[[k]]
      kth_variable_results_new <- jth_results_new[[k]]

      kth_posthoc_old <- kth_variable_results_old$`post hoc with the smallest adj p-values`
      kth_posthoc_new <- kth_variable_results_new$`post hoc with the smallest adj p-values`

      ### 🟩 N_groups ===============================================================================================
      n_groups <- nrow(kth_variable_results_new$`test result as data.frame`)
      # Check difference of p-values
      if (n_groups == 2) {
        if (round(kth_posthoc_new$p.adj, 4) != round(kth_variable_results_new$`test result as data.frame`$adj.p.values[1], 4)) {
          stop("Check the difference of p-values!")
        }
      }

      ### 🟩 check plots ===============================================================================================
      p_old <- kth_variable_results_old$`box plots` # 이전엔 유의했지만, 이번엔 유의하지 않은 경우?
      p_new <- kth_variable_results_new$`box plots`

      # p_old 객체에서 geom_bracket과 stat_bracket을 포함하는 레이어 제거 (boplots의 comparison bar 제거)
      p_new <- remove_layer_if_exists(p_new, "GeomBracket", "StatBracket")

      #### 🟦 유의한 경우 선 추가 =========================================================================
      signif <- kth_variable_results_new$`post hoc with the smallest adj p-values`$p.adj.signif %>% unique()

      if (any(signif %in% c("*", "**", "***", "****"))) {
        response_var <- kth_variable_results_new$`test result as data.frame`$response[1]

        significant_results <- kth_variable_results_new$`post hoc with the smallest adj p-values` %>% dplyr::filter(p.adj <= alpha_posthoc)

        # ggpubr::stat_pvalue_manual을 사용하여 박스플롯에 유의성 표시 추가
        p_new <- p_new + ggpubr::stat_pvalue_manual(
          data = significant_results,
          label = "p.adj.signif",  # 이 열이 별표("***", "**", "*") 유의성 표시를 포함하고 있다고 가정
          y.position = 1.1 * max(df[[response_var]], na.rm = TRUE),  # 유의성 표시 위치
          step.increase = 0.1,  # 선의 높이 조절
          vjust = -0.5  # 세로 위치 조정
        )
      }

      kth_variable_results_new$`box plots` <- p_new
      jth_results_new[[k]] <- kth_variable_results_new
    }
    results.list[[j]] <- jth_results_new
  }


  ## 🟧 return final  =========================================================
  final.list = list(each_var_results = results.list,
                    combined_results_for_reporting = combined_results.list)


  return(final.list)

}
