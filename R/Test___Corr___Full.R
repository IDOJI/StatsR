Test___Corr___Full = function(Data){
  # plot에 표시되는 Adjustment는 글자를 바꿀 수가 없음. 하지만 실제론 내가 선택한 방식으로 적용됨.
  #=============================================================================
  # replace with my function
  #=============================================================================
  assignInNamespace(x = "p.adjust", value = Test___Adjust.p.values, ns = "stats")


  Modified_p_adjust_text = function(x){
    return(x)
  }
  assignInNamespace(x = "p_adjust_text", value = Modified_p_adjust_text, ns = "statsExpressions")
  #=============================================================================
  # ggstatsplot
  #=============================================================================
  # p = ggstatsplot::ggcorrmat(Data,
  #                            cor.vars,
  #                            cor.vars.names,
  #                            matrix.type,
  #                            type,
  #                            tr,
  #                            partial,
  #                            k,
  #                            sig.level,
  #                            conf.level,
  #                            bf.prior,
  #                            p.adjust.method,
  #                            pch,
  #                            ggcorrplot.args,
  #                            package,
  #                            palette,
  #                            colors,
  #                            ggtheme,
  #                            ggplot.component,
  #                            title,
  #                            subtitle,
  #                            caption)


  # First, install and load the required package if you haven't already
  if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
    install.packages("ggstatsplot")
  }

  library(ggstatsplot)

  # Using the `mtcars` dataset for the demonstration
  correlation_plot <- ggstatsplot::ggcorrmat(
    data = mtcars,
    matrix.type = "upper",          # Only show upper half of the matrix
    colors = c("#6D9EC1", "white", "#E46726"),  # Colors for the correlation values
    title = "Correlation Matrix for mtcars dataset",
    subtitle = "with significance stars"
  )

  # Display the plot
  correlation_plot



}


#=============================================================================
# Correlation
#=============================================================================
# ### corr test & plotting
# Corr.Test_results.list = Test___Corr___Plotting.Scatter(data.df, x, y, group,
#                                                         method, alpha,
#                                                         save.path, file.name,
#                                                         expression = F)
# ### Extract = corr test results
# Extracted_results = Test___Corr___Extract.Results(Corr.Test_results.list)
# return(Extracted_results)

# Corr.Test_results = cor.test(data.df[,x] %>% unlist, data.df[,y] %>% unlist, method=method, conf.level=1-alpha)
# Extracted_results = Test___Corr___Extract.Results(Corr.Test_results, alpha, x, y)





