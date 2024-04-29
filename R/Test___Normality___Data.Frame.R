test___normality___data.frame = function(df,
                                         group_var=NULL,
                                         response_var=NULL,
                                         outlier_method = "IQR",
                                         alpha=0.05,
                                         p.adjust.method = "bonferroni"){
  # 🟥Group var #####################################################################################################
  if(!is.null(group_var) && !is.null(response_var)){

    # 데이터 그룹화 및 각 그룹에 대해 정규성 검사 함수 적용

    group_results <- df %>%
      dplyr::group_by(!!sym(group_var)) %>%
      dplyr::summarise(
        result = list(test___normality___single.vector(!!sym(response_var))),
        .groups = 'drop'
      )

    result = group_results$result %>% setNames(group_results[[group_var]])



  # 🟥Non-Group var & Response Var ##################################################################################
  }else if(is.null(group_var) && !is.null(response_var)){

    result = test___normality___single.vector(data %>% pull(!!response_var), outlier_method, alpha) %>% list(response_var = .)

  }






 # 🟥Extract Results ##########################################################################
 results = result %>% test___normality___extract.results(., p.adjust.method)



 # 🟥Return results ##########################################################################
 return(results)
}



