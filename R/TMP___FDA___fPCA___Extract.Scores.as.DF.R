FDA___fPCA___Extract.Scores.as.DF = function(FPCA.list, suffix.for.each.score, path_Export=NULL, file.name){
  # FPCA.list = New_FPCA_AD.list
  # suffix.for.each.score = paste0("ROI_", fit_length(1:length(FPCA.list), 3))

  ### Extracting scores
  Scores.list = lapply(FPCA.list, FUN=function(ith_FPCA){ith_FPCA$scores})
  Scores.df = do.call(cbind, Scores.list) %>% as_tibble


  ### The number of Scores for each ROI
  Selected_Numbers = sapply(Scores.list, FUN=function(x){ncol(x)})



  ### rep by the each number
  repeated_suffix.for.each.score = list()
  Coef_Group_Indicator = list()
  for(i in 1:length(Selected_Numbers)){
    repeated_suffix.for.each.score[[i]] = rep(suffix.for.each.score[i], times = Selected_Numbers[i])
    Coef_Group_Indicator[[i]] = rep(i, times = Selected_Numbers[i])
  }
  Coef_Group_Indicator = Coef_Group_Indicator %>% unlist

  ### Variable names : rep by the each number
  Variables_Names = lapply(repeated_suffix.for.each.score, FUN=function(jth_ROI){
    # jth_ROI = repeated_suffix.for.each.score[[1]]
    paste0(jth_ROI, "_", fit_length(1:length(jth_ROI), 2))
  }) %>% unlist
  names(Scores.df) = Variables_Names


  #=============================================================================
  # Exporting
  #=============================================================================
  Combined_Data = list(PC.Scores = Scores.df, Coef_Group_Indicator = Coef_Group_Indicator)
  if(!is.null(path_Export)){
    dir.create(path_Export, F)
    saveRDS(Combined_Data, file = paste0(path_Export, "/", file.name, ".rds"))
  }


  ### return
  return(Combined_Data)
}














