Test___Corr___Partial = function(Data, x_Vars, y_Vars, type, p.adjust.method, alpha=0.05, x_lab, y_lab, colors, save.path=NULL){
  #=============================================================================
  # Correlation & p.vals
  #=============================================================================
  Corr.df = Test___Corr___Partial___Correlation(Data, x_Vars, y_Vars, type, p.adjust.method, alpha, x_lab, y_lab, save.path)








  #=============================================================================
  # Plotting Corrmat
  #=============================================================================
  p = Test___Corr___Partial___Plotting___Corrmat(Corr.df,
                                                 alpha,
                                                 x_lab,
                                                 y_lab,
                                                 colors,
                                                 save.path)


  return(Corr.df)
}


