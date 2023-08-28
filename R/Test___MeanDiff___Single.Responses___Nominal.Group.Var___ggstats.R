Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats = function(# data & variables
                                                                            Data,
                                                                            Response_Var,
                                                                            Group_Var,
                                                                            alpha_ANOVA,
                                                                            alpha_PostHoc,
                                                                            p.adjust.method,
                                                                            # Tests results
                                                                            is.Normal,
                                                                            is.Equal.Var,
                                                                            # plotting options
                                                                            results.subtitle=F,
                                                                            pairwise.comparisons=T,
                                                                            p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                                                            title="")
{
  #=============================================================================
  # Packges
  #=============================================================================
  install.packages("/Users/Ido/Downloads/ggstatsplot_0.12.0.tar.gz", repos = NULL, type = "source")



  #=============================================================================
  # Arguments
  #=============================================================================
  # adjust p.values
  p.adjust.method = match.arg(p.adjust.method)
  # Normality
  if(is.Normal){
    type = "parametric"
  }else{
    type = "nonparametric"
  }


  ggstatsplot::ggbetweenstats
  p = ggstatsplot::ggbetweenstats(
        data = Data,
        x = !!Group_Var,
        y = !!Response_Var,
        #grouping.var = genre,                           # grouping variable
        k = 4,                                          # number of decimal places for statistical results


        # test type @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        type = type,                                    # which type of test is to be run
        var.equal = is.Equal.Var,


        # outlier & mean @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        outlier.tagging = FALSE,                         # whether outliers need to be tagged
        #outlier.label = !!outlier.label,                # variable to be used for the outlier tag
        #outlier.label.color = "darkgreen",              # changing the color for the text label
        mean.plotting = TRUE,                           # whether mean for each group is to be displayed
        # #mean.ci = TRUE,                                # whether to display confidence interval for means
        mean.label.size = 2.5,                          # size of the label for mean


        # pairwise @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        pairwise.comparisons = pairwise.comparisons,                    # Adjustment method for p-values for multiple comparisons.
        pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
        p.adjust.method = p.adjust.method,              # method for adjusting p-values for multiple comparisons
        pairwise.display = "significant",
        conf.level = 1-alpha_ANOVA,


        # plot @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        # xlab = xlab,                                    # label for the x-axis variable
        # ylab = ylab,                                    # label for the y-axis variable
        title = title,                                  # title text for the plot
        centrality.label.args = list(nudge_x = -0.4),
        ggplot.component = list(ggplot2::theme(plot.title = element_text(size = 20, face = "bold"))),
        notch = FALSE,                                    # show notched box plot
        bf.message = TRUE,                             # add a message with bayes factor favoring null
        # ggtheme = ggthemes::theme_fivethirtyeight(),    # choosing a different theme
        # ggstatsplot.layer = FALSE,                      # turn off ggstatsplot theme layer
        # package = "wesanderson",                        # package from which color palette is to be taken
        # palette = "Darjeeling1",                        # choosing a different color palette
        messages = FALSE,

        # returning statistical results  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        results.subtitle=results.subtitle                           #  Setting it to FALSE will return only the plot.
    )

  return(p)
}


#============================================================================
# Plotting arguments by the criteria
#============================================================================
# type = c("parametric", "p", "robust")



#============================================================================
# Asterisk label
#============================================================================
# Data = pairwise_comparisons(data.Data, !!group, !!variable) %>%
#   dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
#   dplyr::arrange(group1) %>%
#   dplyr::mutate(asterisk_label = c("**", "***", "**"))

