Test___MeanDiff = function(##############################
                           # data & variables
                           ##############################
                           Data,
                           Response_Vars,
                           Group_Var,
                           Group_Var_Type = c("Nominal", "Ordinal"),
                           ##############################
                           # Significance level
                           ##############################
                           alpha_Norm  = 0.05,
                           alpha_Equal.Var  = 0.05,
                           alpha_ANOVA = 0.05,
                           alpha_PostHoc = 0.05,
                           outlier_method = c("IQR"),
                           p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                           type = c("parametric", "nonparametric", "robust", "bayes"),
                           ##############################
                           # Figure
                           ##############################
                           label.as.p.val=F,
                           group.comparison=F,
                           lines.connecting.medians=F,
                           plot_title="",
                           plot_width = 10,
                           plot_height = 7.5,
                           plot_units = "in",
                           plot_dpi = 200,
                           # results.subtitle=T,
                           ##############################
                           # exporting
                           ##############################
                           path_save = NULL,
                           export.separately=TRUE,

                           ...){
  #==================================================================================
  # path
  #==================================================================================
  if(!is.null(path_save)){
    dir.create(path_save, showWarnings = F)
  }







  #==================================================================================
  # type
  #==================================================================================
  type = match.arg(type)






  #==================================================================================
  # 1) Normality
  #==================================================================================
  Results_Normality = Test___Normality(Data, Group_Var, Response_Vars, outlier_method, alpha = alpha_Norm)
  is.Normal = sapply(Results_Normality, function(x) x[[2]])







  #==================================================================================
  # 2) Homoscedasticity
  #==================================================================================
  Results_Homoscedasticity = Test___Equal.Var(Data, Group_Var, Response_Vars, is.Normal, outlier_method, alpha = alpha_Equal.Var)
  is.Equal.Var = sapply(Results_Homoscedasticity, function(x) x[,3])







  #==================================================================================
  # 3) ANOVA
  #==================================================================================
  Group_Var_Type = match.arg(Group_Var_Type)
  if(Response_Vars %>% length > 1){
    Results_ANOVA = Test___MeanDiff___Multi.Reponses(Data, Response_Vars, Group_Var, Group_Var_Type, alpha_ANOVA, is.Normal, is.Equal.Var, type)
  }else{
    Results_ANOVA = Test___MeanDiff___Single.Responses(Data,
                                                       Response_Vars,
                                                       Group_Var,
                                                       Group_Var_Type,
                                                       alpha_ANOVA,
                                                       p.adjust.method,
                                                       is.Normal,
                                                       is.Equal.Var,
                                                       type,
                                                       plot_title="")
  }
  cat("\n", crayon::green("Testing"), crayon::red("Mean Differece"), crayon::green("is done!"),"\n")





  #==================================================================================
  # 4) Combine results
  #==================================================================================
  if(! 3 * length(Response_Vars) == length(Results_Normality) + length(Results_Homoscedasticity) + length(Results_ANOVA$Results)){
    stop("There is a variable which is not done yet")
  }

  Combined_Results.list = list(Normality = Results_Normality,
                               Homoscedasticity = Results_Homoscedasticity,
                               ANOVA = Results_ANOVA)

  Combined_Results.df = lapply(seq_along(Response_Vars), function(k){
    ccbind(Results_Normality[[k]]$Norm_results, Results_Homoscedasticity[[k]]) %>%
      ccbind(., Results_ANOVA$Results[[k]])
  }) %>% setNames(Response_Vars) %>% as_tibble()






  #==================================================================================
  # 4) Export ANOVA Results as data frame
  #==================================================================================
  if(!is.null(path_save)){
    file.name = paste0("[ANOVA] Results_", "`", Group_Var, "`")

    # save a combined df
    # Test___MeanDiff___Export.xlsx.Highlight(Combined_Results.df,
    #                                         path_save,
    #                                         file.name,
    #                                         Group_Var_Type)
    cat("\n", crayon::green("Exporting"), crayon::red("Mean Difference Results"), crayon::green("is done!"),"\n")
  }





  #==================================================================================
  # 5) Boxplot
  #==================================================================================
  if(!is.null(path_save)){
    Result_Boxplots = Results_ANOVA$Boxplots

    for(b in 1:length(Result_Boxplots)){

      file.name = paste0("[Boxplot] Results_", "`", Group_Var, "`___", "`", Response_Vars[b], "`")


      # plot_height, plot_width에 대한 옵션 넣어야 함
      ggsave(plot = Result_Boxplots[[b]],
             path = path_save,
             filename = paste0(file.name, ".png"),
             bg = "white",
             width = plot_width,
             height = plot_height,
             units = plot_units,
             dpi = plot_dpi)
    }
    cat("\n", crayon::green("Exporting"), crayon::red("Boxplots"), crayon::green("is done!"),"\n")
  }




  #==================================================================================
  # 6) Save combined result
  #==================================================================================
  if(!is.null(path_save)){
    file.name = paste0("[ANOVA] Combined_Results_", "`", Group_Var, "`")

    # save RDS file
    saveRDS(object = Combined_Results.list, file = paste0(path_save, "/", file.name, ".rds"))

    cat("\n", crayon::green("Exporting"), crayon::red("Combined Results"), crayon::green("is done!"),"\n")
  }


  # # Combining tables for LaTeX
  # Combined.list = lapply(list.files(path_save, pattern = "@_Combined Results for Latex Table", full.names=T), read.csv)
  # First_Cols = Combined.list[[1]][,1:3]
  # Second_Cols = lapply(Combined.list, function(x) x[,-c(1:3)])
  # Combined_New.list = c(First_Cols, Second_Cols)
  # Combined_New.df = do.call(cbind, Combined_New.list)
  # Which_rows_to_highlight = apply(Combined_New.df, 1, function(x){
  #   x = Combined_New.df[1,] %>% unlist
  #   only_having_ns = sum(x[-c(1:3)] %in% c("none", "HNS", "NS")) == length(x)
  #   return(!only_having_ns)
  # }) %>% which
  # Export___xlsx___Highlighting(Combined_New.df, colors.list = "red", which_cols.list = 1:ncol(Combined_New.df), coloring_index.list = Which_rows_to_highlight, path_save = path_save, file.name = "@@_Combined Results for Latex Table", sheet.name = "")
  # write.csv(Combined_New.df, paste0(path_save, "/@@_Combined Results for Latex Table", ".csv"), row.names=F)
  #




  # Combining p.values
  # Combined.list = lapply(list.files(path_save, pattern = "@_Only p.values Combined Results", full.names=T), read.csv)
  # write.csv(do.call(rbind, Combined.list), paste0(path_save, "/@@_Only p.values Combined Results", ".csv"), row.names=F)








  #==================================================================================
  # 5) Boxplot
  #==================================================================================
  # Boxplot.list = lapply(Results_ANOVA, function(ith_Results){
  #   Test___MeanDiff___Single.Responses___Box.Plot(Data,
  #                                                 ith_Results,
  #                                                 label.as.p.val,
  #                                                 group.comparison,
  #                                                 lines.connecting.medians,
  #                                                 path_save)
  # })

  return(Combined_Results.list)
}



