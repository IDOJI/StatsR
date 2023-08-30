Test___Corr = function(Data,
                       x_Vars = NULL,
                       y_Vars = NULL,
                       x_lab = "",
                       y_lab = "",
                       # cor.vars = NULL,
                       # cor.vars.names = NULL,
                       # matrix.type = "upper",
                       type = "parametric",
                       # tr = 0.2,
                       # partial = FALSE,
                       # k = 2L,
                       # sig.level = 0.05,
                       # conf.level = 0.95,
                       # bf.prior = 0.707,
                       # p.adjust.method = "TSBH",
                       # pch = "cross",
                       # ggcorrplot.args = list(method = "square",
                       #                        outline.color = "black",
                       #                        pch.cex = 14),
                       # package = "RColorBrewer",
                       # palette = "Dark2",
                       colors = c("#E69F00", "white", "#009E73"),
                       # ggtheme = ggstatsplot::theme_ggstatsplot(),
                       # ggplot.component = NULL,
                       # title = NULL,
                       # subtitle = NULL,
                       # caption = NULL
                       ...){
  # plot에 표시되는 Adjustment는 글자를 바꿀 수가 없음. 하지만 실제론 내가 선택한 방식으로 적용됨.
  # colors = c("#E69F00", "white", "#009E73")
  # colors = c("blue", "white", "red")
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





  #=============================================================================
  # partial correlation
  #=============================================================================
  if(!is.null(x_Vars) && !is.null(y_Vars)){

  }







}


Test___Corr___Partial = function(Data, x_Vars, y_Vars, type, p.adjust.method, alpha=0.05, x_lab, y_lab, colors, save.path=NULL){
  #=============================================================================
  # Correlation & p.vals
  #=============================================================================
  Corr.df = Test___Corr___Partial___Correlation(Data, x_Vars, y_Vars, type, p.adjust.method, alpha)



  #=============================================================================
  # Plotting Corrmat
  #=============================================================================
  Corr.list$p = Test___Corr___Partial___Plotting___Corrmat(Corr.df,
                                                           alpha,
                                                           x_lab,
                                                           y_lab,
                                                           colors,
                                                           save.path)





  #=============================================================================
  # Exporting xlsx
  #=============================================================================
  Corr.list$Corr_Sub.mat


  return()
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





