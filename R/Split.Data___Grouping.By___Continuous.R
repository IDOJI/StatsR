Split.Data___Grouping.By___Continuous = function(Data, Var_Name, Cut_Points, Eq = c("<=", "<"), New_Var_Name = NULL, groups=NULL){
  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("dplyr", "rlang"), load=TRUE)






  #=============================================================================
  # Cut points
  #=============================================================================
  # 각 cut_point에 대해서 <= 가 default값
  # Cut_Points = c(18,25,35,45,55,65,75)
  # Data = Data %>% mutate(Age_Group = case_when(
  #   Age <= 20 ~ "0-18",
  #   Age <= 40 ~ "18-30",
  #   Age <= 60 ~ "30-40",
  #   Age <= 80 ~ "50-60",
  #   TRUE ~ "60+"
  # ))







  #=============================================================================
  # Create the labels for the "Var_Name" groups
  #=============================================================================
  Eq = match.arg(Eq)

  # The first
  The_First_Label = paste0("x", Eq, Cut_Points[1])

  # The middle
  The_Middle_Labels = sapply(2:length(Cut_Points), function(i, ...){
    if(Eq == "<="){
      paste0(Cut_Points[i-1], "<", "x", Eq, Cut_Points[i])
    }else if(Eq == "<"){
      paste0(Cut_Points[i-1], "<=", "x", Eq, Cut_Points[i])
    }
  })

  # The last
  if(Eq == "<="){
    The_Last_Label = paste0("x", ">", tail(Cut_Points, 1))
  }else if(Eq == "<"){
    The_Last_Label = paste0("x", ">=", tail(Cut_Points, 1))
  }

  # Combine
  if(!is.null(groups) && length(groups) == length(Labels)){
    Labels = c(The_First_Label, The_Middle_Labels, The_Last_Label)
  }else{
    # If groups are provided, replace the default labels
    Labels = groups
  }
  Labels = factor(Labels, levels=Labels)








  #=============================================================================
  # Create the conditions for the "Var_Name" groups
  #=============================================================================
  Conditions = lapply(seq_along(Cut_Points), function(i, ...){
    if(Eq == "<="){
      if(i == 1){
        ith_result = quo(!!sym(Var_Name) <= !!Cut_Points[i] ~ !!Labels[i])
      }else{
        ith_result = quo(!!sym(Var_Name) > !!Cut_Points[i-1] & !!sym(Var_Name) <= !!Cut_Points[i] ~ !!Labels[i])
      }
    }else if(Eq == "<"){
      if(i == 1){
        ith_result = quo(!!sym(Var_Name) < !!Cut_Points[i] ~ !!Labels[i])
      }else{
        ith_result = quo(!!sym(Var_Name) >= !!Cut_Points[i-1] & !!sym(Var_Name) < !!Cut_Points[i] ~ !!Labels[i])
      }
    }
    return(ith_result)
  })
  Conditions = c(Conditions,
                 list(quo(TRUE ~ !!tail(Labels, 1))))





  #=============================================================================
  # Use the mutate and case_when functions to create the New Var group
  #=============================================================================
  Data = Data %>% mutate(!!sym(New_Var_Name) := case_when(!!!Conditions))




  return(Data)
}





