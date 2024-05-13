test___mean.diff___multiple = function(df,
                                       group_vars,
                                       group_var_type = "nomial",
                                       response_vars,
                                       is.paired = FALSE,
                                       alpah_anova = 0.05,
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

      return(x)
    }) %>% setNames(names(ith_results.list))

    results.list[[i]] = ith_results_new.list
  }
  names(combined_results.list) = names(results.list)



  ## 🟧 return final  =========================================================
  final.list = list(each_var_results = results.list,
                    combined_results_for_reporting = combined_results.list)


  return(final.list)

}
