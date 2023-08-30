Test___Corr___Partial___Plotting___Corrmat = function(Corr.df,
                                                      alpha,
                                                      x_lab,
                                                      y_lab,
                                                      colors,
                                                      save.path,
                                                      ...){





  #=============================================================================
  # Visualization
  #=============================================================================
  p = ggplot(data = Corr.df, aes(x = Var2, y = Var1)) +
    # Tile for coloring
    geom_tile(aes(fill = value), color = "white") +
    # Overlaying 'X' for non-significant coefficients with larger size
    geom_text(aes(label = ifelse(Adj_p.value > alpha, "X", "")), size = 13, color = "green") +
    # Displaying coefficients
    geom_text(aes(label = round(value, 2)), size = 4) +
    scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3],
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name=paste0(method, "\nCorrelation")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_text(size = 16, face="bold"),  # Adjust size as needed
          axis.title.y = element_text(size = 16, face="bold")) +  # Adjust size as needed
    labs(x = x_lab, y = y_lab)



  #=============================================================================
  # Exporting
  #=============================================================================
  if(!is.null(save.path)){
    ggsave(filename = paste0(save.path, "/[Correlation] ", x_lab, " vs ",  y_lab, ".png"), plot = p, dpi = 300)
  }


  return(p)
}
