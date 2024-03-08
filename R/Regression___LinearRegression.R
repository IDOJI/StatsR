Regression___LinearRegression = function(df, y, X=NULL){
  # 🟥 Fitting linear model ============================================================================
  fit = lm(SUB___as.formula(y, X), df)


  # 🟥 Extract Results ==================================================================================
  Results.list = list()
  Results.list$Regression_Results = Regression___LinearRegression___ExtractResults(fit)



  # 🟥 Plotting ==================================================================================
  if(is.null(X)){

    X = colnames(df)[colnames(df)!=y]

    Results.list$Scatter_Plots = lapply(X, function(ith_X){
      ggplot___scatterplot(df = df, x = ith_X, y = y)
    }) %>% setNames(X)

  }




  # 🟥 Return ==================================================================================
  return(Results.list)
}















