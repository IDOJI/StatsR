Test___Corr = function(Data,
                       x_Vars = NULL,
                       y_Vars = NULL,
                       alpha=0.05,
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
                       p.adjust.method = "TSBH",
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
                       # caption = NULL,
                       save.path = NULL,
                       ...)
{
  # colors = c("#E69F00", "white", "#009E73")
  # colors = c("blue", "white", "red")
  #=============================================================================
  # save.path
  #=============================================================================
  if(!is.null(save.path)){
    dir.create(save.path, F)
  }




  #=============================================================================
  # Correlation  & plotting
  #=============================================================================
  # partial
  if(!is.null(x_Vars) && !is.null(y_Vars)){
    Corr.df = Test___Corr___Partial(Data, x_Vars, y_Vars, type, p.adjust.method, alpha, x_lab, y_lab, colors, save.path)
  }else{
    # Full
    Corr.df = Test___Corr___Full(Data)
  }


  #=============================================================================
  # Return
  #=============================================================================
  return(Corr.df)
}






















