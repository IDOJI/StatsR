ggplot___histogram___group = function(df,
                                      x,
                                      group_var,
                                      group_combined=F,
                                      density = T,
                                      same_colors_density = F){
  # 🟥 Change colnames #########################################################
  # df = data
  df = change_colnames(df, x, "x_Var")
  df = change_colnames(df, group_var, "group_Var")
  group_legend = group_var
  group_var = NULL




  # 🟥 color palette ################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  # 'sample' 함수 사용 시, 'replace = TRUE'로 설정하여 색상을 반복할 수 있게 함
  # 가능한 모든 팔레트에서 색상 추출
  all_colors <- unlist(sapply(c("Pastel1", "Pastel2", "Set1", "Set2", "Set3"), function(p) {
    brewer.pal(8, p)
  }))

  # 그룹 수 계산
  n_group <- df$group_Var %>% unique %>% length

  # 그룹 수가 색상 수를 초과하는지 확인
  if (n_group > length(all_colors)) {
    stop("Not enough unique colors available for the number of groups.")
  }

  # 필요한 그룹 수만큼 색상 샘플링
  colors_hist <- sample(all_colors, n_group)


  # 'Dark2' 팔레트를 사용하여 색상 인터폴레이터 생성
  dark_colors_interpolator <- colorRampPalette(brewer.pal(8, "Dark2"))

  # 그룹 수에 맞는 색상 생성
  colors_density <- dark_colors_interpolator(n_group)





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

    p_density = p_density + geom_density(aes(color = factor(group_Var), fill = factor(group_Var)), alpha = 0.3, linewidth = 1) +
      labs(title = "Histogram and Density") +
      scale_color_manual(values = colors_hist, guide = guide_legend(title = group_legend))

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
