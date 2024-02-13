Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats = function(Data,
                                                                            Response_Var,
                                                                            Group_Var,
                                                                            alpha_ANOVA,
                                                                            # Tests results
                                                                            is.Normal,
                                                                            is.Equal.Var,
                                                                            type = c("parametric", "nonparametric", "robust", "bayes"),
                                                                            plot_title = "",
                                                                            outlier.tagging = FALSE,
                                                                            tr=0.2,
                                                                            # plotting options
                                                                            # results.subtitle = T,
                                                                            # pairwise.comparisons = T,
                                                                            # p.adjust.method = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY")
                                                                            ...)
{
  #=============================================================================
  # Packges
  #=============================================================================
  # install.packages("/Users/Ido/Downloads/ggstatsplot_0.12.0.tar.gz", repos = NULL, type = "source")
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?
    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }
  install_packages("ggthemes")






  #=============================================================================
  # type
  #=============================================================================
  type = match.arg(type)
  # Normality
  if(!type %in% c("bayes", "robust")){
    if(is.Normal){
      type = "parametric"
    }else{
      type = "nonparametric"
    }
  }





  #=============================================================================
  # pallette
  #=============================================================================
  # install_packages("RColorBrewer")
  # # Step 1: Generate palette
  # all_colors <- brewer.pal(12, "Set3")  # 12 is the maximum for Set3
  #
  # # Step 2: Filter out undesired color
  # filtered_colors <- all_colors[all_colors != "#FFFFB3"]
  #
  # # Step 3: Check if you need more colors
  # n_colors <- Data[, Group_Var] %>% unlist %>% unique %>% length
  #
  # if (length(filtered_colors) < n_colors) {
  #   # This is just an example: you might want to add a color or generate colors in another way
  #   filtered_colors <- c(filtered_colors, "#FF0000")
  # }
  #
  # # Use 'filtered_colors' in your plot
  # colors <- filtered_colors[1:n_colors]










  #=============================================================================
  # test
  #=============================================================================
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
    ggplot.component = list(ggplot2::theme(plot.title = element_text(size = 20, face = "bold"))),
    results.subtitle = TRUE,
    title = plot_title,
    package = "yarrr", ## package from which color palette is to be taken
    palette = "info2", ## choosing a different color palette



    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # test
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    conf.level = 1-alpha_ANOVA,
    pairwise.comparisons = TRUE,
    pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
    p.adjust.method = "none"                       # method for adjusting p-values for multiple comparisons

  ) %>% suppressWarnings()



  return(p)
}
#
#
# #============================================================================
# # Plotting arguments by the criteria
# #============================================================================
# # type = c("parametric", "p", "robust")
#
#
#
# #============================================================================
# # Asterisk label
# #============================================================================
# # Data = pairwise_comparisons(data.Data, !!group, !!variable) %>%
# #   dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
# #   dplyr::arrange(group1) %>%
# #   dplyr::mutate(asterisk_label = c("**", "***", "**"))
#

















#=============================================================================
# Boxplot
#=============================================================================
# p1 <- ggpubr::ggboxplot(data = Data,
#                         x = Group_Var,
#                         y = `Response_Var`,
#                         color = Group_Var,
#                         palette = colors,
#                         # shape = Group_Var,
#                         size = 0.5,
#                         add = "jitter",
#                         add.params = list(size=0.5))
#






#
#
# p = ggstatsplot::ggbetweenstats(
#
#   #grouping.var = genre,                           # grouping variable
#   k = 4,                                          # number of decimal places for statistical results
#
#
#
#
#
#
#
#   # pairwise @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#   ,                    # Adjustment method for p-values for multiple comparisons.
#   pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
#   p.adjust.method = p.adjust.method,              # method for adjusting p-values for multiple comparisons
#   pairwise.display = "significant",
#   conf.level = 1-alpha_ANOVA,
#
#
#   # plot @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#   # xlab = xlab,                                    # label for the x-axis variable
#   # ylab = ylab,                                    # label for the y-axis variable
#   title = title,                                  # title text for the plot
#   centrality.label.args = list(nudge_x = -0.4),
#
#   notch = FALSE,                                    # show notched box plot

#   # ggstatsplot.layer = FALSE,                      # turn off ggstatsplot theme layer
#   # package = "wesanderson",                        # package from which color palette is to be taken
#   # palette = "Darjeeling1",                        # choosing a different color palette
#   messages = FALSE,
#
#   # returning statistical results  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# )
#















#
# install.packages("gapminder")
# require(gapminder)
#
# p = ggbetweenstats(
#   data = Data,
#   x = !!Group_Var, ## grouping/independent variable
#   y = !!Response_Var, ## dependent variables
#   type = "parametric", ## type of statistics
#   xlab = "Continent", ## label for the x-axis
#   ylab = "Life expectancy", ## label for the y-axis
#   ## turn off messages
#   ggtheme = ggplot2::theme_gray(), ## a different theme
#   package = "yarrr", ## package from which color palette is to be taken
#   palette = "info2", ## choosing a different color palette
#   title = "Comparison of life expectancy across continents (Year: 2007)",
#   caption = "Source: Gapminder Foundation"
# ) + ## modifying the plot further
#   ggplot2::scale_y_continuous(
#     limits = c(35, 85),
#     breaks = seq(from = 35, to = 85, by = 5)
#   )
#
# ggstatsplot::extract_stats(p)

## selecting subset of the data
# df_year <- dplyr::filter(gapminder::gapminder, year == 2007 | year == 1957)
# #
# p1 <- ggbetweenstats(
#   data = df_year,
#   x = year,
#   y = lifeExp,
#   xlab = "Year",
#   ylab = "Life expectancy",
#   # to remove violin plot
#   violin.args = list(width = 0),
#   type = "p",
#   conf.level = 0.99,
#   title = "Parametric test",
#   package = "ggsci",
#   palette = "nrc_npg"
# )
#
# p2 <- ggbetweenstats(
#   data = df_year,
#   x = year,
#   y = lifeExp,
#   xlab = "Year",
#   ylab = "Life expectancy",
#   # to remove box plot
#   boxplot.args = list(width = 0),
#   type = "np",
#   conf.level = 0.99,
#   title = "Non-parametric Test",
#   package = "ggsci",
#   palette = "uniform_startrek"
# )
#
# p3 <- ggbetweenstats(
#   data = df_year,
#   x = year,
#   y = lifeExp,
#   xlab = "Year",
#   ylab = "Life expectancy",
#   type = "r",
#   conf.level = 0.99,
#   title = "Robust Test",
#   tr = 0.005,
#   package = "wesanderson",
#   palette = "Royal2",
#   k = 3
# )
#
# ## Bayes Factor for parametric t-test and boxviolin plot
# p4 <- ggbetweenstats(
#   data = df_year,
#   x = year,
#   y = lifeExp,
#   xlab = "Year",
#   ylab = "Life expectancy",
#   type = "bayes",
#   violin.args = list(width = 0),
#   boxplot.args = list(width = 0),
#   point.args = list(alpha = 0),
#   title = "Bayesian Test",
#   package = "ggsci",
#   palette = "nrc_npg"
# )
#
# ## combining the individual plots into a single plot
# combine_plots(
#   list(p1, p2, p3, p4),
#   plotgrid.args = list(nrow = 2),
#   annotation.args = list(
#     title = "Comparison of life expectancy between 1957 and 2007",
#     caption = "Source: Gapminder Foundation"
#   )
# )














