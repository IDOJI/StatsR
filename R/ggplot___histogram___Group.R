ggplot___histogram___Group = function(df,
                                      x,
                                      group_var,
                                      group_combined=F,
                                      density = T,
                                      same_colors_density = F){
  # 🟥 Change colnames #########################################################
  df = change_colnames(df, x, "x_Var")
  df = change_colnames(df, group_var, "group_Var")
  group_legend = group_var
  group_var = NULL




  # 🟥 color palette ################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  colors = brewer.pal(n = 12, sample(palette, 1)) %>% suppressWarnings()
  n_group = df$group_Var %>% unique() %>% length()
  colors_hist = sample(colors, n_group, replace=F)
  colors_density = brewer.pal(n_group, "Dark2")[1:n_group] %>% suppressWarnings()




  # 🟥 plotting : basic ################################################################
  p <- ggplot(df, aes(x = x_Var, fill = factor(group_Var))) +
    geom_histogram(aes(y = after_stat(density)), color = "black", bins = 20, alpha = 0.5) +
    scale_fill_manual(values = colors_hist, guide = guide_legend(title = group_legend)) +
    theme_minimal() +
    labs(x = x, title = "Histogram", fill = group_legend) +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold")
    )




  # 🟥 plotting : density ################################################################
  p_density = p

  if(density){

    p_density = p_density + geom_density(aes(color = factor(group_Var), fill = factor(group_Var)), alpha = 0.3, size = 1) +
      labs(title = "Histogram and Density") +
      scale_color_manual(values = colors_density, guide = guide_legend(title = group_legend))

      # same colors with histogram?
      if(same_colors_density){

        p_density = p_density + scale_fill_manual(values = colors_hist, guide = guide_legend(title = group_legend)) +
          scale_color_manual(values = colors_hist, guide = guide_legend(title = group_legend))

      }

  }





  # 🟥 plotting : not Group combined? ################################################################
  p_group = p_density

  if(!group_combined){

    p_group = p_group + facet_wrap(~ group_Var, scales = "free")  # 그룹별 히스토그램 그룹화

  }




  # 🟥 return ################################################################
  return(p_group)
}
