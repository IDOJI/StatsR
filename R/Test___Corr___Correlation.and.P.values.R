Test___Corr___Correlation.and.P.values = function(Data, x_Vars, y_Vars, type){
  #=============================================================================
  # Select method
  #=============================================================================
  if(type == "parametric"){
    method = "pearson"
  }else if(type == "nonparametric"){
    method = "spearman"
  }


  #=============================================================================
  # Compute Correlation & its p-value
  #=============================================================================
  # cor_matrix = Data[, c(x_Vats, y_Vars)] %>% cor(method = method)
  install_packages("Hmisc")
  Result = as.matrix(Data[, c(x_Vars, y_Vars)]) %>% Hmisc::rcorr(type = method)
  Result.list = list(Correlation = Result$r, P.values = Result$P)


  return(Result.list)
}
?correlation::correlation





hist(iris$Sepal.Length, main = "Original data")

hist(winsorize(iris$Sepal.Length, threshold = 0.2),
     xlim = c(4, 8), main = "Percentile Winsorization"
)
