ggplot___histogram___Nongroup = function(df,
                                         x = NULL,
                                         density = T){
  # 🟥 Change colnames #########################################################
  df = change_colnames(df, x, "x_Var")



  # 🟥 color palette ################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  colors = brewer.pal(n = 12, sample(palette, 1)) %>% suppressWarnings()
  fill_color = sample(colors, 1)



  # 🟥 Histogram ################################################################
  p <- ggplot(df, aes(x = x_Var)) +
    geom_histogram(aes(y = after_stat(density)), fill = fill_color, color = "black", bins = 20) +  # 밀도 기준으로 높이 조정 및 랜덤 색상 설정
    theme_minimal() +
    labs(x = x, title = "Histogram") +
    theme(
      axis.title = element_text(size = 14, face = "bold"),  # 축 제목 설정
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # 플롯 타이틀 설정
    )




  # 🟥 Density ################################################################
  if(density){

    # Random color
    p = p + geom_density(alpha = 0.2, fill = sample(colors, 1)) +
      labs(x = x, title = "Histogram with Density")

  }


  return(p)
}




