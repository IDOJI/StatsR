Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats___plotting = function(Data,
                                                                                       Response_Var,
                                                                                       Group_Var,
                                                                                       alpha_ANOVA,
                                                                                       # Tests results
                                                                                       is.Normal,
                                                                                       is.Equal.Var,
                                                                                       type = c("parametric", "nonparametric", "robust", "bayes"),
                                                                                       plot_title = "Boxplot",
                                                                                       outlier.tagging = FALSE,
                                                                                       tr=0.2,
                                                                                       results.subtitle = T,
                                                                                       pvalues=NULL,
                                                                                       # plotting options
                                                                                       # results.subtitle = T,
                                                                                       # pairwise.comparisons = T,
                                                                                       # p.adjust.method = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY")
                                                                                       ...){
  # 🟥 Plot1 =============================================================================
  p = ggstatsplot::ggbetweenstats(
    data = Data,
    x = !!Group_Var,
    y = !!Response_Var,


    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # Test type
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    type = type,                                    # which type of test is to be run
    var.equal = is.Equal.Var,
    tr=tr,
    bf.message = TRUE,                             # add a message with bayes factor favoring null



    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # outlier & mean
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    outlier.tagging = outlier.tagging,                         # whether outliers need to be tagged
    outlier.label = !!Response_Var,                # variable to be used for the outlier tag
    outlier.label.color = "darkgreen",              # changing the color for the text label
    mean.plotting = TRUE,                            # whether mean for each group is to be displayed
    mean.ci = TRUE,                               # whether to display confidence interval for means
    mean.label.size = 2.5,                           # size of the label for mean
    messages = TRUE,




    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # plotting
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ggplot.component = list(ggplot2::theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))),
    results.subtitle = results.subtitle,
    title = plot_title,
    package = "yarrr", ## package from which color palette is to be taken
    palette = "info2", ## choosing a different color palette



    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # test
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    conf.level = 1-alpha_ANOVA,
    pairwise.comparisons = TRUE,
    pairwise.display = "s",
    pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
    p.adjust.method = "none")                        # method for adjusting p-values for multiple comparisons








  # 🟥 p value bar ####################################################################
  # Extract p-val
  Results = ggstatsplot::extract_stats(p)

  if(is.null(pvalues) && results.subtitle){
    pvalues = Results$subtitle_data$p.value
  }

  if(pvalues <= alpha_ANOVA){

    Groups = p$data[[1]] %>% unique %>% as.character


    if(length(Groups) > 2){
      ## 🟨 more than 2 groups =================================================================
      # Extract comparison results
      Comparison = Results$pairwise_comparisons_data %>% as.data.frame

      # Extract significant cases only
      Significant_pairs = Comparison[Comparison$p.value <= alpha_ANOVA, ]

      # generate `group` variable
      Significant_pairs = Significant_pairs %>%
        dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
        dplyr::arrange(group1)

      # plotting
      by = max(p$data[[2]]) * 0.25 / 6
      p2 = p + geom_signif(comparisons = Significant_pairs$groups,
                           y_position = max(p$data[[2]]) + seq(0, 10, by = by)[1:length(Groups)],
                           test.args = list(exact = FALSE),
                           annotations = SUB___P.vals.Signif.Stars(Significant_pairs$p.value)
      )




    }else if(length(Groups) == 2){
      ## 🟨 2 groups =================================================================
      p2 = p + geom_signif(comparisons = list(Groups),
                           test.args = list(exact = FALSE),
                           annotations = SUB___P.vals.Signif.Stars(pvalues)
      )

    }else{

      stop("Check the number of groups")

    }

  }else{

    p2 = p

  }


  return(p2)
}
