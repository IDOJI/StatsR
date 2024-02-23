ggplot___histogram___Group = function(df,
                                      x,
                                      group_var,
                                      group_combined=F,
                                      density = T){
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
    scale_fill_manual(values = colors_hist) +  # 색상 지정
    theme_minimal() +
    labs(x = x, title = "Histogram", fill = group_legend) +
    theme(
      axis.title = element_text(size = 14, face = "bold"),  # 축 제목 설정
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 플롯 타이틀 설정
      legend.title = element_text(size = 12, face = "bold")
    )





  # 🟥 plotting : density ################################################################
  p_density = p
  if(density){

    p_density = p_density + geom_density(aes(color = factor(group_Var), fill = factor(group_Var)), alpha = 0.3, size = 1) +
      scale_color_manual(values = colors_density) +
      labs(x = x, title = "Histogram and density")

  }




  p <- ggplot(df, aes(x = x_Var, group = group_Var)) +
    geom_histogram(aes(y = after_stat(density), fill = factor(group_Var)),
                   color = "black",
                   bins = 20,
                   alpha = 0.5) +  # 밀도 기준으로 높이 조정 및 랜덤 색상 설정
    scale_fill_manual(values = colors_hist) +  # 색상 지정
    theme_minimal() +
    labs(x = x,
         title = "Histogram",
         fill = group_legend) +
    theme(
      axis.title = element_text(size = 14, face = "bold"),  # 축 제목 설정
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 플롯 타이틀 설정
      legend.title = element_text(size = 12, face = "bold")
    )



  # 🟥 plotting : not Group combined? ################################################################
  if(!group_combined){

    p = p + facet_wrap(~ group_Var, scales = "free")  # 그룹별 히스토그램 그룹화

  }



  # 🟥 plotting : density ################################################################



    ggplot(df, aes(x = x_Var, fill = group_Var)) +
      geom_histogram(aes(y = after_stat(density)), color = "black", bins = 20, alpha = 0.5) +
      geom_density(alpha = 0.3, color = "blue") +  # 밀도 곡선 추가
      theme_minimal() +
      labs(x = "X축 이름", title = "Histogram with Density") +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )

    # 플롯 출력
    print(p)



    p = ggplot(df, aes(x = x_Var)) +
      geom_histogram(aes(y = ..density.., fill = group_Var), color = "black", bins = 20, alpha = 0.5) +
      scale_fill_manual(values = hist_colors) +
      scale_color_manual(values = density_colors) +
      facet_wrap(~ group_Var, scales = "free_y") +  # 각 그룹별로 패널을 분할
      theme_minimal() +
      labs(x = x,
           title = "Histogram by Group",
           fill = group_Var,  # fill 범례 제목 변경
           color = group_Var) +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        # legend.title = element_text(size = 12, face = "bold")  # 범례 제목의 글자 크기 및 스타일 조절
        legend.position = "none"  # 범례 없애기
      )



    if(density){

      colors = brewer.pal(n = 12, sample("Dark2", 1)) %>% suppressWarnings()
      density_colors = sample(colors, length(unique(df$group_var)))

      p + geom_density(aes(colour = density_colors, fill = density_colors), alpha = 0.3, size = 1) +
        labs(x = x, title = "Histogram with Density by Group")

      # 밀도 추가
      p <- p + geom_density(aes(color = group_var, fill = group_var), alpha = 0.3, size = 1) +
        scale_color_manual(values = density_colors) +
        scale_fill_manual(values = density_colors)



    }
  }

  # 🟥 plotting : Density ################################################################


  return(p)

}



 ggplot(df, aes(x = x_Var)) +
  geom_histogram(aes(fill = group_Var), color = "black", bins = 20, alpha = 0.5) +
  scale_fill_manual(values = hist_colors) +  # 히스토그램의 색상을 지정합니다.
  facet_wrap(~ group_Var, scales = "free") +  # 각 그룹별로 패널을 분할합니다.
  theme_minimal() +
  labs(x = x,
       title = "Histogram with Density by Group",
       fill = "Group") +  # 히스토그램의 범례 제목을 변경합니다.
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  geom_density(aes(color = group_Var), size = 1, alpha = 0.3) +  # density plot을 추가합니다.

  labs(color = "Group")  # density plot의 범례 제목을 변경합니다.










library(ggplot2)
library(RColorBrewer)

# 데이터 프레임 예시 생성
set.seed(123) # 일관된 결과를 위한 시드 설정
df <- data.frame(x_Var = rnorm(200), group_var = sample(letters[1:3], 200, replace = TRUE))

# 히스토그램 색상 설정
hist_colors <- setNames(brewer.pal(3, "Pastel1"), unique(df$group_var))

# 밀도 색상 설정
density_colors <- setNames(brewer.pal(3, "Dark2"), unique(df$group_var))

# ggplot 객체 생성


# 플롯 출력
print(p)




