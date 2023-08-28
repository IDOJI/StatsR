Test___MeanDiff___Export.xlsx.Highlight___With.Post.Hoc = function(){
  colors_PostHoc = rep("#F1948A", length(names_PostHoc)) %>% as.list


  ### which cols
  names_MeanDiff = list()
  names_MeanDiff[[1]] = filter_by(names(Results.df), include = c("Group", "Trt"), exact_include = F,ignore.case = F)
  names_MeanDiff[[2]] = filter_by(names(Results.df), include = c("Response"), exact_include = F, ignore.case = F)
  names_MeanDiff[[3]] = filter_by(names(Results.df), include = c("MeanDiff", "p.val"), exact_include = F, ignore.case=F)
  names_MeanDiff = names_MeanDiff %>% unlist %>% as.list

  names_PostHoc = list()
  names_PostHoc[[1]] = filter_by(names(Results.df), include = c("group"), exact_include = F, ignore.case=F)
  names_PostHoc[[2]] = filter_by(names(Results.df), include = c("PostHoc"), exact_include = F, ignore.case=F)
  names_PostHoc = names_PostHoc %>% unlist %>% as.list



  ### what colors
  colors_MeanDiff = rep("#ABEBC6", length(names_MeanDiff)) %>% as.list




  ### which rows
  rows_MeanDiff = which(Results.df$MeanDiff_p.value <= alpha_ANOVA)
  rows_MeanDiff.list = rep(list(rows_MeanDiff), length(names_MeanDiff))

  rows_PostHoc = which(Results.df$PostHoc_p.value_adj <= alpha_PostHoc)
  rows_PostHoc.list = rep(list(rows_PostHoc), length(names_PostHoc))
}


