Test___Correlation___Check.Method = function(X, Y, method, alpha, outlier_method){
  #🟥 The sample size ##########################################################
  N_obs = nrow(X)
  is.large = N_obs>=30



  #🟥 Normality ################################################################
  ##🟨 X normal? ===============================================================
  X_Norm = apply(X, MARGIN=2, function(x){
    Test___Normality___Single.Vector(unlist(x), outlier_method, alpha)$is.Normal
  })

  if(sum(X_Norm) != ncol(X)){
    is.normal.X = FALSE
  }else{
    is.normal.X = TRUE
  }


  ##🟨 Y normal? ===============================================================
  is.normal.Y = Test___Normality___Single.Vector(Y %>% unlist, outlier_method, alpha)$is.Normal


  #🟥 Decision ################################################################
  Limiting_Distribution_of_MME = is.large
  Joint_pdf_Normal = is.normal.Y && is.normal.X
  if(Limiting_Distribution_of_MME || Joint_pdf_Normal){
    if(method == "pearson"){
      method = "pearson"
    }else{
      method = "spearman"
      warning("The sample size is small or These variables are not Normal distributed. Check if using Pearson is correct. In this case, Spearman is used.")
    }
  }


  #🟥 Return ################################################################
  return(method)
}
