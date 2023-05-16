Test___MeanDiff___Exporting.XLSX.Highlight = function(Reporting.df,
                                                      alpha_ANOVA,
                                                      alpha_PostHoc,
                                                      save.path,
                                                      filename){
  ### create directory
  dir.create(save.path, showWarnings = F)


  #==============================================================================================
  # Selecting cols : Reporting df
  #==============================================================================================
  ### which cols
  names_MeanDiff = list()
  names_MeanDiff[[1]] = filter_by(names(Reporting.df), including.words = c("Group", "Trt"), ignore.case = F)
  names_MeanDiff[[2]] = filter_by(names(Reporting.df), including.words = c("Response"), ignore.case = F)
  names_MeanDiff[[3]] = filter_by(names(Reporting.df), including.words = c("MeanDiff", "p.val"))
  names_MeanDiff = names_MeanDiff %>% unlist %>% as.list

  names_PostHoc = list()
  names_PostHoc[[1]] = filter_by(names(Reporting.df), including.words = c("group"), ignore.case=F)
  names_PostHoc[[2]] = filter_by(names(Reporting.df), including.words = c("PostHoc"))
  names_PostHoc = names_PostHoc %>% unlist %>% as.list



  ### what colors
  colors_MeanDiff = rep("#ABEBC6", length(names_MeanDiff)) %>% as.list
  colors_PostHoc = rep("#F1948A", length(names_PostHoc)) %>% as.list



  ### which rows
  rows_MeanDiff = which(Reporting.df$MeanDiff_p.value <= alpha_ANOVA)
  rows_MeanDiff.list = rep(list(rows_MeanDiff), length(names_MeanDiff))

  rows_PostHoc = which(Reporting.df$PostHoc_p.value_adj <= alpha_PostHoc)
  rows_PostHoc.list = rep(list(rows_PostHoc), length(names_PostHoc))


  Export___xlsx___Highlighting(data.df             = Reporting.df,
                               colors.list         = c(colors_MeanDiff, colors_PostHoc),
                               which_cols.list     = c(names_MeanDiff, names_PostHoc),
                               coloring_index.list = c(rows_MeanDiff.list, rows_PostHoc.list),
                               save.path           = save.path ,
                               file_name           = filename,
                               sheet.name          = "ANOVA Results")


}

