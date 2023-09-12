Test___MeanDiff___Single.Responses___Ordinal.Group.Var = function(Data,
                                                                  Response_Vars,
                                                                  Group_Var,
                                                                  alpha_ANOVA = 0.05,
                                                                  p.adjust.method,
                                                                  is.Normal=NULL,
                                                                  is.Equal.Var=NULL,
                                                                  type){
  #==================================================================================
  # Test for each response
  #==================================================================================
  Results.list = lapply(seq_along(Response_Vars), function(i){
    if(is.Normal[i]){
      Test___MeanDiff___Single.Responses___Ordinal.Group.Var___Parametric()
    }else{
      Test___MeanDiff___Single.Responses___Ordinal.Group.Var___Nonparametric(Data, Group_Var, Response_Var = Response_Vars[i])
    }
  })
  names(Results.list) = Response_Vars







  #=============================================================================
  # 1) two.sided p-values adjustment & decision
  #=============================================================================
  # 각 변수들에 대한 ANOVA 결과에 대한 p값 추출 후
  # p값 보정
  p.values_two.sided = sapply(Results.list, function(y){
    y$p.value[1]
  })
  adjusted_p.values_two.sided = Test___Adjust.p.values(p.values_two.sided, method = p.adjust.method, alpha = alpha_ANOVA)
  # 보정 된 p값에서 어떤 변수에 대해 유의했는가
  which_two.sided_signif = which(adjusted_p.values_two.sided < alpha_ANOVA)










  #=============================================================================
  # 2) one-sided
  #=============================================================================
  Results_Which_Signif.list = lapply(seq_along(Results.list), function(k){
    y = Results.list[[k]]
    y$Which_Significant = ""

    # Two-sided에서 유의한가?
    if(k %in% which_two.sided_signif){
      # yes : which side?
      index_min_p.val = which.min(y$p.value[2:3]) + 1
      y$Which_Significant[index_min_p.val] = "This"
    }else{
      # 유의하지 않으면 two-sided의 pvalue 사용
      y$Which_Significant[1] = "This"
    }
    return(y)
  })




  #=============================================================================
  # 위에서 선택된 p값으로 p값 다시 보정
  #=============================================================================
  selected_p.values = sapply(Results_Which_Signif.list, function(y){
    y$p.value[which(y$Which_Significant=="This")]
  })
  adjusted_seleceted_p.values = Test___Adjust.p.values(selected_p.values, method = p.adjust.method, alpha = alpha_ANOVA)

  # 보정된 p값 추가
  Results_Which_Signif.list = lapply(seq_along(Results_Which_Signif.list), function(k){
    y = Results_Which_Signif.list[[k]]
    y$p.value_adj = ""
    y$p.value_adj[which(y$Which_Significant=="This")] = adjusted_seleceted_p.values[k]
    y
  })





  #=============================================================================
  # data frame 정렬
  #=============================================================================
  Results_Combined.list = lapply(Results_Which_Signif.list, function(y){
    # y = Results_Which_Signif.list[[1]]
    p.values = y$p.value %>% matrix(nrow=1) %>% as.data.frame %>% setNames(c("p.value_two.sided", "p.value_increasing", "p.value_decreasing"))
    y$p.value = NULL


    y_selected = y[which(y$Which_Significant=="This"), ]

    y_combined = cbind(y_selected, p.values) %>% select(-Which_Significant)


    # which_p.val = grep(y_combined$alternative, names(y_combined))

    return(y_combined)
  })
  Results_Combined.df = do.call(rbind, Results_Combined.list)





  #=============================================================================
  # Which p.val_adj significant
  #=============================================================================
  Results_Combined.df$Significance = SUB___P.vals.Signif.Stars(p.vals = Results_Combined.df$p.value_adj)






  #=============================================================================
  # Add variables names
  #=============================================================================
  Results_Combined.df$Responses = Response_Vars
  Results_Combined.df$Group = Group_Var
  Results_Combined.df = Results_Combined.df %>% relocate(Group, Responses)







  # #=============================================================================
  # # Combining Resulst
  # #=============================================================================
  # Combined_Results = do.call(rbind, Minimal_p.vals)
  # Combined_Results = cbind(Group = Group_Var, Response = Response_Vars, Combined_Results)
  # rownames(Combined_Results) = NULL
  # Combined_Results.list = split(Combined_Results, seq_len(nrow(Combined_Results)))
  # names(Combined_Results.list) = Response_Vars



  return(Results_Combined.df)
}















