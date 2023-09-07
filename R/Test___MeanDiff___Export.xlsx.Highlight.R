Test___MeanDiff___Export.xlsx.Highlight = function(Results_ANOVA, save.path, file.name, export.separately=TRUE){
  #=============================================================================
  # Combine results for variables
  #=============================================================================
  if(!export.separately){
    Results_ANOVA = list(do.call(rbind, Results_ANOVA))
  }








  #=============================================================================
  # Combine results for variables
  #=============================================================================
  Combined.list = list()
  for(k in 1:length(Results_ANOVA)){
    cat("\n",crayon::bgRed(k),"\n")
    kth_Results_ANOVA = Results_ANOVA[[k]]
    Have_Post.Hoc = grep("Post.Hoc", names(kth_Results_ANOVA))

    Signif_rows = which(!kth_Results_ANOVA$Significance %in% c("HNS", "NS")) %>% as.list
    kth_Results_ANOVA = apply(kth_Results_ANOVA, 2, as.character) %>% as.data.frame  # exporting xlsx 때문에 character로 전부 변환



    kth_Results_ANOVA_Selected_Cols = kth_Results_ANOVA %>% select(all_of(c("parameter1",	"parameter2", "group1", "group2", "Significance")))
    Combined.list[[k]] = kth_Results_ANOVA_Selected_Cols
    Combined.list[[k]] = Combined.list[[k]] %>% rename(!!Combined.list[[k]]$parameter1[1] := "Significance") %>% select(-parameter1)



    if(length(Signif_rows)!=0){
      # Export total results
      Export___xlsx___Highlighting(data.df             = kth_Results_ANOVA,
                                   colors.list         = "#ABEBC6",
                                   which_cols.list     = 1:ncol(kth_Results_ANOVA),
                                   coloring_index.list = Signif_rows,
                                   save.path           = save.path,
                                   file.name           = paste0(names(Results_ANOVA)[k], "___Total___", file.name),
                                   sheet.name          = "ANOVA Results")




      # Export only significant case
      if(Signif_rows[[1]]!=1){
        Signif_rows_New = c(1, Signif_rows) %>% unlist
      }else{
        Signif_rows_New  = Signif_rows %>% unlist
      }
      Export___xlsx___Highlighting(data.df             = kth_Results_ANOVA_Selected_Cols,
                                   colors.list         = "#ABEBC6",
                                   which_cols.list     = 1:ncol(kth_Results_ANOVA_Selected_Cols),
                                   coloring_index.list = Signif_rows_New,
                                   save.path           = save.path,
                                   file.name           = paste0(names(Results_ANOVA)[k], "___Selected_Cols___", file.name),
                                   sheet.name          = "ANOVA Results")
    }else{
      write.csv(kth_Results_ANOVA, file = paste0(names(Results_ANOVA)[k], "___Selected_Cols___", file.name, ".csv"), na = "", row.names = F)
    }


    # Export only significant case : Selected columns
  }






  #=============================================================================
  # Combine results for variables
  #=============================================================================
  # Combine the list elements
  combined.df = Reduce(function(x, y) {
    merge(x, y, by = c("parameter2", "group1", "group2"))
  }, Combined.list)
  write.csv(combined.df, file = paste0(save.path, "/Combined Results for Latex Table___", file.name, ".csv") ,na = "", row.names = F)






  #=============================================================================
  # Only p.values
  #=============================================================================







}




















# if(length(Have_Post.Hoc)!=0){
#   Test___MeanDiff___Export.xlsx.Highlight___With.Post.Hoc(Results.df, save.path, file.name)
# }else{
#   Test___MeanDiff___Export.xlsx.Highlight___Without.Post.Hoc(Results.df, save.path, file.name)
# }
