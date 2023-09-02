Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor___More.Groups = function(p,
                                                                                                    Group_Var,
                                                                                                    Response_Var){
  #===========================================================================
  # Extract results
  #===========================================================================
  Results = ggstatsplot::extract_stats(p)
  Mean.Diff = Results$subtitle_data
  Post.Hoc = Results$pairwise_comparisons_data %>% rename("p.value_Comparison" := "p.value")




  #===========================================================================
  # binding
  #===========================================================================
  Combined.df = ccbind(Mean.Diff, Post.Hoc)




  return(Combined.df)
}



# Mean.Diff_results = Mean.Diff_results.list$subtitle_data
# Mean.Diff_results_Cols = names(Mean.Diff_results)
#
# if("parameter1" %in% Mean.Diff_results_Cols && "parameter2" %in% Mean.Diff_results_Cols){
#   Mean.Diff_results = Mean.Diff_results %>% dplyr::rename("Response" := "parameter1")
#   Mean.Diff_results = Mean.Diff_results %>% dplyr::rename("Group(Trt)" := "parameter2")
# }else{
#   Mean.Diff_results = dplyr::bind_cols(Response_Var, Group_Var, Mean.Diff_results)
#   names(Mean.Diff_results)[1:2] = c("Response", "Group(Trt)")
# }
# Mean.Diff_results = Mean.Diff_results %>% dplyr::rename("MeanDiff_p.value":="p.value")
# Mean.Diff_results = Mean.Diff_results %>% dplyr::rename("MeanDiff_statistic":="statistic")
# Mean.Diff_results = Mean.Diff_results %>% dplyr::rename("MeanDiff_Method":="method")
# Mean.Diff_results$MeanDiff_p.value.signif = SUB___P.vals.Signif.Stars(Mean.Diff_results$MeanDiff_p.value)
# Mean.Diff_results = Mean.Diff_results %>% dplyr::relocate(MeanDiff_p.value.signif, .after = MeanDiff_Method)
#
#
#
#
#
# #===========================================================================
# # Comparison results
# #===========================================================================
# Comparison_results = Mean.Diff_results.list$pairwise_comparisons_data
# if(!is.null(Comparison_results)){
#   Comparison_results = Mean.Diff_results.list$pairwise_comparisons_data
#   Comparison_results = Comparison_results %>% dplyr::rename("PostHoc_p.value_adj":="p.value")
#   if("statistics" %in% names(Comparison_results)){
#     Comparison_results = Comparison_results %>% dplyr::rename("PostHoc_statistic":="statistic")
#   }
#   Comparison_results = Comparison_results %>% dplyr::rename("PostHoc_Test":="test")
#   Comparison_results = dplyr::bind_cols(Comparison_results, PostHoc_p.value.signif = SUB___P.vals.Signif.Stars(Comparison_results$PostHoc_p.value_adj))
# }
#
