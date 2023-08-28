Test___MeanDiff___Single.Responses___Ordinal.Group.Var___Nonparametric = function(Data,
                                                               Group_Var,
                                                               Response_Var){
  #=============================================================================
  # packages
  #=============================================================================
  install_packages("clinfun")





  #=============================================================================
  # Sort data
  #=============================================================================
  Group = Data %>% select(all_of(Group_Var)) %>% unlist() %>% unname()
  x = Data %>% select(all_of(Response_Var)) %>% unlist() %>% unname()

  Order = order(Group)
  x_Ordered = x[Order]
  Group_Ordered = Group[Order] %>% as.numeric



  #=============================================================================
  # Jonckheere-Terpstra test
  #=============================================================================
  # H0(귀무가설) : 환자 증상에 따라 스트레스 수치 중앙값은 같다. M1 = M2 = M3
  # H1(대립가설) : 환자 증상에 따라 스트레스 수치는 커진다. M1 ≤ M2 ≤ M3 -> alternative = "increasing"
  # alternative = two.sided -> increasing 이거나 decreasing 이거나 (유의수준은 각각 절반)
  alternative = c("two.sided", "increasing", "decreasing")

  method = c()
  statistic = c()
  p.vals = c()


  Results.list = lapply(alternative, function(ith_alternative){
    ith_Results = clinfun::jonckheere.test(x = x_Ordered, g = Group_Ordered, ith_alternative) %>% suppressWarnings()

    method <<- c(method, ith_Results$method)
    statistic <<- c(statistic, ith_Results$statistic)
    p.vals <<- c(p.vals, ith_Results$p.value)
    return(ith_Results)
  })






  #=============================================================================
  # Combine Results
  #=============================================================================
  Combined_Results = data.frame(method, statistic, alternative, p.vals)


  cat("\n",crayon::green("Testing"), crayon::red("Jonckheere-Terpstra"), crayon::green("is done!"), "\n")
  return(Combined_Results)
}



#=============================================================================
# Post-hoc -> 의미 없을 듯
#=============================================================================
# Groups = unique(Group) %>% as.numeric %>% sort
# Group_Combination = apply(combn(Groups, 2), 2, function(x){list(x)})
# Post.Hoc = lapply(Group_Combination, function(y){
#   Group_Index = Group_Ordered %in% unlist(y)
#   Group_Ordered_Subset = Group_Ordered[Group_Index]
#   x_Ordered_Subset = x_Ordered[Group_Index]
#   clinfun::jonckheere.test(x = x_Ordered_Subset, g = Group_Ordered_Subset)
# })
