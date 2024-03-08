Test___Correlation = function(df, y, x=NULL, alpha = 0.05,
                              method = "pearson",
                              outlier_method="IQR",
                              p.adj.method = "bonferroni", ...) {
  # 🟥 Arguments #####################################################################################################
  ## 🟨 x = colnames (Character) =====================================================================================
  if(is.null(x)){
    x = names(df)[names(df)!=y]
  }

  ## 🟨 x,y = colnames (Character) =====================================================================================
  Data = Test___Correlation___Check.x.y.Input(df, y, x)
  X = Data$X
  Y = Data$Y





  ## 🟨 Method =====================================================================================
  method =  Test___Correlation___Check.Method(X, Y, method, alpha, outlier_method)




  # 🟥Results List ################################################################################
  Results.list = list()





  # 🟥Correlation Tests #########################################################################
  ## 🟨Corr test for each variable ===========================================================
  Correlation_Test = apply(X, MARGIN=2, function(x){

    ith_Corr_Test = cor.test(x = unlist(x), y = unlist(Y), method = method) %>%
      unlist() %>%
      t() %>%
      as.data.frame()

    Character_Vars = c("alternative", "method", "data.name")
    Numeric_Vars = names(ith_Corr_Test)[!names(ith_Corr_Test) %in% Character_Vars]

    for(ith_Num_Var in Numeric_Vars){
      ith_Corr_Test = change_class(df = ith_Corr_Test, cols = ith_Num_Var, what_class = "numeric")
    }

    return(ith_Corr_Test)

  }) %>% do.call(rbind, .)


  ## 🟨Add x var names ===========================================================
  Correlation_Test_2 = row.names(Correlation_Test) %>% cbind(Variables = ., Correlation_Test)
  rownames(Correlation_Test_2) = NULL



  ## 🟨Adjust pvals ===========================================================
  Adjsted_pvals = SUB___P.vals.Adjust(Correlation_Test$p.value, method = p.adj.method, alpha = alpha)
  Results.list$Correlation_Test = cbind(Correlation_Test_2, Adjsted_pvals)





  # 🟥Visualization #########################################################################
  ## 🟨Corrleation plot ============================================================================
  Results.list$Plot$Total = ggplot___correlation(df, y, x, method, p.adj.method)



  ## 🟨Scatter plot ============================================================================
  Results.list$Plot$Scatter = lapply(x, function(ith_x){
    ggplot___scatterplot(df, ith_x, y, method)
  }) %>% setNames(x)



  # 🟥Results #########################################################################
  return(Results.list)

}




















