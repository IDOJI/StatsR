Test___Corr___Partial___Correlation = function(Data, x_Vars, y_Vars, type, p.adjust.method, alpha, x_lab, y_lab, save.path=NULL){
  #=============================================================================
  # type
  #=============================================================================
  if(type == "parametric"){
    method = "Pearson"
  }else if(type == "nonparametric"){
    method = "Spearman"
  }




  #=============================================================================
  # Correlation & P-values
  #=============================================================================
  Corr.list = Test___Corr___Correlation.and.P.values(Data, x_Vars, y_Vars, type)
  Corr.mat = Corr.list$Correlation
  Pval.mat = Corr.list$P.values







  #=============================================================================
  # Extract only the submatrix that corresponds to correlations between 'x_Vars' and 'y_Vars'
  #=============================================================================
  Index_rows = rownames(Corr.mat) %in% x_Vars
  Index_cols = colnames(Corr.mat) %in% y_Vars

  Corr_Sub.mat = Corr.mat[Index_rows, Index_cols] %>% t()
  Pval_Sub.mat = Pval.mat[Index_rows, Index_cols] %>% t()







  #=============================================================================
  # Adjust p.vals
  #=============================================================================
  Adj.Pval_Sub.mat = Pval_Sub.mat
  Adj.Pval_Sub.mat[] = Test___Adjust.p.values(as.vector(Pval_Sub.mat), method = p.adjust.method, alpha)






  #=============================================================================
  # Packages
  #=============================================================================
  install_packages(c("ggplot2", "reshape2"))







  #=============================================================================
  # Melt the matrix for ggplot2
  #=============================================================================
  melted_Corr_Sub.df = reshape2::melt(Corr_Sub.mat)
  melted_Corr_Sub.df$p.value = as.vector(Pval_Sub.mat)
  melted_Corr_Sub.df$method = ifelse(type=="parametric", "Pearson",
                                     ifelse(type=="nonparametric", "Spearman", "None"))





  #=============================================================================
  # Significance matrix
  #=============================================================================
  melted_Adj.Pval_Sub.df = reshape2::melt(Adj.Pval_Sub.mat)
  # # Melt the significance matrix to match the format of melted_cor
  # melted_Signif_Adj.Pval_Sub.mat <- melt(Signif_Adj.Pval_Sub.mat)
  # # Add the significance column to melted_cor
  # melted_cor$significant <- melted_significance$value




  #=============================================================================
  # Combining Results
  #=============================================================================
  melted_Corr_Sub.df$p.adjust.method = p.adjust.method
  melted_Corr_Sub.df$Adj_p.value = melted_Adj.Pval_Sub.df$value
  melted_Corr_Sub.df$Significance = SUB___P.vals.Signif.Stars(p.vals = melted_Corr_Sub.df$Adj_p.value)







  #=============================================================================
  # export xlsx
  #=============================================================================
  if(!is.null(save.path)){
    Export___xlsx___Highlighting(data.df             = melted_Corr_Sub.df,
                                 colors.list         = "#ABEBC6",
                                 which_cols.list     = names(melted_Corr_Sub.df),
                                 coloring_index.list = which(melted_Corr_Sub.df$Adj_p.value <= alpha),
                                 save.path           = save.path ,
                                 file.name           = paste0("[", method, " Correlation] ", x_lab, " vs ", y_lab),
                                 sheet.name          = "Correlation Results")

  }




  #=============================================================================
  # Returning
  #=============================================================================
  return(melted_Corr_Sub.df)
}
