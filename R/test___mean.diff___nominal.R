# test___mean.diff___nominal = function(df,
#                                       response_var,
#                                       group_var,
#                                       alpha_anova = 0.05,
#                                       is.normal,
#                                       is.equal.var,
#                                       ...){
#   # 🟥 p.adjust.method =============================================================================
#
#   1) is.normal, is.equal.var로 정규성과 등분산성을 판단한다.
#   2) df라는 데이터프레임을 데이터로, response_var는 반응변수, group_var는 그룹변수의 이름을 담고 있다.
# 위내용을 토대로 p.adjust.method에 다음 값들 가운데 하나를 넣는 코드를 만들어.
# #  1. `holm`: Holm's method
# # 2. `hochberg`: Hochberg's method
# #  3. `hommel`: Hommel's method
# # 4. `bonferroni`: Bonferroni correction
# # 5. `BH` or `fdr`: Benjamini & Hochberg's method to control the false discovery rate
# #  7. `BY`: Benjamini & Yekutieli's method, which is a modification of the Benjamini & Hochberg method
# # 8. `none`: No adjustment (use raw p-values)
# #
# #  정규성을 만족하고, 등분산성을 만족하지 않고, 그룹 간에 표본이 불균등한 경우
# #  Games-Howell
# p.adjust.method
# #
# #
# #
# #
# #   p.adjust.method =
# require(ggstatsplot)
# require(ggplot2)
#
#   # 🟥 Test =============================================================================
#   p = ggstatsplot::ggbetweenstats(
#     data = df,
#     x = !!group_var,
#     y = !!response_var,
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # Test type
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # which type of test is to be run
#     type = ifelse(is.normal, "parametric", "nonparametric"),
#     centrality.type = ifelse(is.normal, "parametric", "nonparametric"),
#     var.equal = is.equal.var,
#     # add a message with bayes factor favoring null
#     bf.message = FALSE,
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # outlier & mean
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # outlier.tagging = outlier.tagging,                         # whether outliers need to be tagged
#     outlier.label = !!response_var,                # variable to be used for the outlier tag
#     outlier.label.color = "darkgreen",              # changing the color for the text label
#     mean.plotting = TRUE,                            # whether mean for each group is to be displayed
#     mean.ci = TRUE,                               # whether to display confidence interval for means
#     mean.label.size = 2.5,                           # size of the label for mean
#     messages = FALSE,
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # plotting
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     ggplot.component = list(ggplot2::theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))),
#     results.subtitle = TRUE,
#     # title = plot_title,
#     package = "yarrr",## package from which color palette is to be taken
#     palette = "info2",## choosing a different color palette
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     # test
#     #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#     conf.level = 1-alpha_anova,
#     pairwise.comparisons = TRUE,
#     pairwise.display = "s",
#     pairwise.annotation = "p.value",## how do you want to annotate the pairwise comparisons
#     p.adjust.method = p.adjust.method## method for adjusting p-values for multiple comparisons
#     )
#
#
#
#
#   # ANOVA 수행
#   anova_result <- aov(mpg ~ cyl, data = mtcars)
#   summary(anova_result)
#   # 데이터 필터링
#   data_4cyl <- mtcars$mpg[mtcars$cyl == 4]
#   data_6cyl <- mtcars$mpg[mtcars$cyl == 6]
#
#   # 데이터의 길이 맞추기 (실제 적용 시, 적절한 쌍을 이루는 데이터 필요)
#   min_length <- min(length(data_4cyl), length(data_6cyl))
#   data_4cyl <- data_4cyl[1:min_length]
#   data_6cyl <- data_6cyl[1:min_length]
#
#   # Paired t-test 수행
#   t.test(data_4cyl, data_6cyl, paired = TRUE)
#
#   # 🟥 p value bar ####################################################################
#   # Extract p-val
#   results = ggstatsplot::extract_stats(p)
#   comparison = results$pairwise_comparisons_data
#
#   View(comparison)
#
#
#
#
#
#   if(is.null(pvalues) && results.subtitle){
#     pvalues = Results$subtitle_data$p.value
#   }
#
#   if(pvalues <= alpha_ANOVA){
#
#     Groups = p$data[[1]] %>% unique %>% as.character
#
#
#     if(length(Groups) > 2){
#       ## 🟨 more than 2 groups =================================================================
#       # Extract comparison results
#       Comparison = Results$pairwise_comparisons_data %>% as.data.frame
#
#       # Extract significant cases only
#       Significant_pairs = Comparison[Comparison$p.value <= alpha_ANOVA, ]
#
#       # generate `group` variable
#       Significant_pairs = Significant_pairs %>%
#         dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
#         dplyr::arrange(group1)
#
#       # plotting
#       by = max(p$data[[2]]) * 0.25 / 6
#       p2 = p + geom_signif(comparisons = Significant_pairs$groups,
#                            y_position = max(p$data[[2]]) + seq(0, 10, by = by)[1:length(Groups)],
#                            test.args = list(exact = FALSE),
#                            annotations = SUB___P.vals.Signif.Stars(Significant_pairs$p.value)
#       )
#
#
#
#
#     }else if(length(Groups) == 2){
#       ## 🟨 2 groups =================================================================
#       p2 = p + geom_signif(comparisons = list(Groups),
#                            test.args = list(exact = FALSE),
#                            annotations = SUB___P.vals.Signif.Stars(pvalues)
#       )
#
#     }else{
#
#       stop("Check the number of groups")
#
#     }
#
#   }else{
#
#     p2 = p
#
#   }
#   # 🟥 plotting with results =============================================================================
#   plot_with_results = Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats___plotting(df,
#                                                                                                   response_var,
#                                                                                                   group_var,
#                                                                                                   alpha_anova,
#                                                                                                   # Tests results
#                                                                                                   is.normal,
#                                                                                                   is.equal.var,
#                                                                                                   type = type,
#                                                                                                   plot_title = plot_title,
#                                                                                                   outlier.tagging = outlier.tagging,
#                                                                                                   tr=tr,
#                                                                                                   results.subtitle = T)
#
#
#
#   # 🟥 plotting with results =============================================================================
#   plot_without_results = Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats___plotting(df,
#                                                                                                      response_var,
#                                                                                                      group_var,
#                                                                                                      alpha_anova,
#                                                                                                      # Tests results
#                                                                                                      is.normal,
#                                                                                                      is.equal.var,
#                                                                                                      type = type,
#                                                                                                      plot_title = plot_title,
#                                                                                                      outlier.tagging = outlier.tagging,
#                                                                                                      tr=tr,
#                                                                                                      results.subtitle = F,
#                                                                                                      pvalues = ggstatsplot::extract_stats(plot_with_results)[[1]][,"p.value"]
#   )
#
#
#
#
#
#   ggstats.list = lapply(seq_along(response_vars), function(k){
#     test___mean.diff___single.responses___nominal.group.var___ggstats(df,
#                                                                       response_var = response_vars[k],
#                                                                        group_var,
#                                                                        alpha_anova,
#                                                                        is.normal = is.normal[k],
#                                                                        is.equal.var = is.equal.var[k],
#                                                                        type,
#                                                                        plot_title,
#                                                                        outlier.tagging,
#                                                                        tr)
#   }) %>% setNames(response_vars)
#
#
#
#
#
#
#
#   # 🟥 Extract Results ###########################################################################
#   Extracted_Results.list = lapply(seq_along(ggstats.list), function(k){
#
#     kth_ANOVA = Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor(p = ggstats.list[[k]]$plot_with_results,
#                                                                                            df,
#                                                                                            group_var,
#                                                                                            response_var = response_vars[k])
#     kth_ANOVA$Used.Significance.Level = alpha_anova
#     kth_ANOVA$Group.Difference = kth_ANOVA$p.value < alpha_anova
#     return(kth_ANOVA)
#   }) %>% setNames(response_vars)
#
#
#
#
#
#   # 🟥 save boxplots ###########################################################################
#   if(!is.null(path_save)){
#
#     for(b in seq_along(response_vars)){
#
#       file.name = paste0("[Boxplot] Results_", "`", group_var, "`___", "`", response_vars[b], "`")
#
#       # plot_height, plot_width에 대한 옵션 넣어야 함
#       dir.create(paste0(path_save, "/Boxplot"),showWarnings = F)
#       ggsave(plot = ggstats.list[[b]]$plot_without_results,
#              path = paste0(path_save, "/Boxplot"),
#              filename = paste0(file.name, ".png"),
#              bg = "white",
#              width = plot_width,
#              height = plot_height,
#              units = plot_units,
#              dpi = plot_dpi)
#     }
#
#     cat("\n", crayon::green("Exporting"), crayon::red("Boxplots"), crayon::green("is done!"),"\n")
#   }
#
#
#
#
#
#
#   # 🟥 Combine ###########################################################################
#   Variables = names(ggstats.list)
#
#   Combined_Results.list = lapply(seq_along(Variables), function(k){
#
#
#     list(Boxplot = ggstats.list[[k]], Result = Extracted_Results.list[[k]])
#
#   }) %>% setNames(Variables)
#
#
#
#
#   # 🟥 return ###########################################################################
#   Combined_Results.list %>% return
# }
#
#
#
#
#
#
# #==================================================================================
# # ANOVA results for each groups
# #==================================================================================
# # MeanDiff_Multi_Responses.list = lapply(seq_along(var_responses), FUN=function(k, ...){
# #   ith_results = Test___MeanDiff___Single.Responses___Nominal.Group.Var(df,
# #                                                                        var_group,
# #                                                                        var_response = var_responses[k],
# #                                                                        alpha_Norm,
# #                                                                        alpha_Equal.Var,
# #                                                                        alpha_anova,
# #                                                                        alpha_PostHoc,
# #                                                                        p.adjust.method,
# #                                                                        save.path,
# #                                                                        filename = paste0("[ANOVA_Boxplot] ", '`', var_responses_filename[k],"`"," by `", var_group_filename, "`"),
# #                                                                        export.xlsx = T,
# #                                                                        Boxplot_label.as.p.val = Boxplot_label.as.p.val)
# #   cat("\n",  crayon::blue("The response variable"), crayon::red(var_responses[k]), crayon::blue("is done!"), "\n")
# #   return(ith_results)
# # })
#
#
# #
# #
# # Test___MeanDiff_Multi = function(#################################################
# #                                  # df & variabels
# #                                  #################################################
# #                                  dfset.df,
# #                                  variables,
# #                                  group_variables,
# #                                  #################################################
# #                                  # significance level
# #                                  #################################################
# #                                  norm_alpha=0.05,
# #                                  anova_alpha ,
# #                                  posthoc_alpha,
# #                                  p.adjust.method="bonferroni",
# #                                  #################################################
# #                                  # path & file names
# #                                  #################################################
# #                                  group_filenames,
# #                                  path,
# #                                  file_name = "MeanDiff"){
# #
# #
# #   #==============================================================================
# #   # Meandiff results for each group variable
# #   #==============================================================================
# #
# #   MeanDiff_results.list = lapply(group_variables, FUN=function(ith_group_variable, ...){
# #     ind = which(ith_group_variable==group_variables)
# #
# #     ith_filename = group_filenames[ind]
# #
# #     Final.list = lapply(variables, FUN=function(v, ...){
# #       title = paste(v, "by", g,sep=" ")
# #       filename = paste(v, "_", f, ".png", sep="")
# #       Test___MeanDiff(#################################################
# #                       # dfset
# #                       #################################################
# #                       X                 =    dfset.df,
# #                       group             =    ith_group_variable,
# #                       variable          =    v,
# #                       #################################################
# #                       # significance level
# #                       #################################################
# #                       norm_alpha        =    norm_alpha,
# #                       anova_alpha       =    anova_alpha,
# #                       posthoc_alpha     =    posthoc_alpha,
# #                       p.adjust.method   =    p.adjust.method,
# #                       #################################################
# #                       # filename
# #                       #################################################
# #                       title             =    title,
# #                       path              =    path,
# #                       filename          =    filename)
# #     })
# #
# #
# #
# #     return(Final_results.df)
# #   })
# #
# #
# #
# #
# #   #################################################
# #   # combining results
# #   #################################################
# #   for(i in 1:length(Final.list)){
# #     if(i==1){
# #       Final_results.df = Final.list[[i]]
# #     }else{
# #       Final_results.df = rrbind(Final_results.df, Final.list[[i]])
# #     }
# #   }
# #
# #
# #   ### combining
# #   for(i in 1:length(MeanDiff_results.list)){
# #     if(i==1){
# #       MeanDiff.df = MeanDiff_results.list[[i]]
# #     }else{
# #       MeanDiff.df = rrbind(MeanDiff.df, MeanDiff_results.list[[i]])
# #     }
# #   }
# #
# #
# #
# #
# #
#
# #
# #
# #
# #   return(MeanDiff.df)
# # }
#
#
