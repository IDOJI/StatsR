ggplot___histogram___Group = function(df,
                                      x,
                                      group_var,
                                      group_combined=F,
                                      density = T){
  # 🟥 Change colnames #########################################################
  df = change_colnames(df, x, "x_Var")
  df = change_colnames(df, group_var, "group_var")




  # 🟥 color palette ################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  colors = brewer.pal(n = 12, sample(palette, 1)) %>% suppressWarnings()
  hist_colors <- sample(colors, length(unique(df$group_var)), replace=F)






  # 🟥 plotting ################################################################
  if(group_combined){

    # 색상도 다르게  옵션 /?
    # Density + Hist combined
    p <- ggplot(df, aes(x = x_Var, group = group_var)) +
      geom_histogram(aes(y = ..density.., fill = factor(group_var)), color = "black", bins = 20, alpha = 0.5) +
      geom_density(aes(color = factor(group_var)), size = 1, alpha = 0.3) +
      scale_fill_manual(values = group_colors) +
      scale_color_manual(values = group_colors) +
      theme_minimal() +
      labs(x = x,
           title = "Histogram with Density by Group",
           fill = group_var,  # fill 범례 제목 변경
           color = group_var) +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12, face = "bold")  # 범례 제목의 글자 크기 및 스타일 조절
      )



  }else{

    p <- ggplot(df, aes(x = x_Var)) +
      geom_histogram(aes(fill = group_var, y = ..density..), color = "black", bins = 20, alpha = 0.5) +
      scale_fill_manual(values = hist_colors) +
      # geom_density(aes(color = group_var), size = 1, alpha = 0.7) +
      # scale_color_manual(values = density_colors) +
      theme_minimal() +
      labs(x = "X Variable", title = "Histogram with Density by Group", fill = "Group Var", color = "Group Var") +
      theme(legend.position = "right")

    library(ggplot2)
    library(RColorBrewer)

    # 데이터 프레임 예시 생성
    set.seed(123) # 일관된 결과를 위한 시드 설정
    df <- data.frame(x_Var = rnorm(200), group_var = sample(letters[1:3], 200, replace = TRUE))

    # 히스토그램과 밀도 플롯에 사용할 색상 정의
    hist_colors <- brewer.pal(3, "Pastel1")
    density_colors <- brewer.pal(3, "Dark2")

    # ggplot 객체 생성 및 그룹별 히스토그램 그리기
    p <- ggplot(df, aes(x = x_Var, fill = group_var)) +
      geom_histogram(aes(y = ..density..), color = "black", bins = 20, alpha = 0.5) +
      geom_density(aes(color = group_var), size = 1, alpha = 0.7) +
      scale_fill_manual(values = hist_colors) +
      scale_color_manual(values = density_colors) +
      facet_wrap(~ group_var, scales = "free") +
      theme_minimal() +
      labs(x = "X Variable", title = "Histogram with Density by Group") +
      theme(legend.position = "none")

    # 플롯 출력
    print(p)



    p = ggplot(df, aes(x = x_Var)) +
      geom_histogram(aes(y = ..density.., fill = group_var), color = "black", bins = 20, alpha = 0.5) +
      scale_fill_manual(values = group_colors) +
      scale_color_manual(values = colors) +
      facet_wrap(~ group_var, scales = "free_y") +  # 각 그룹별로 패널을 분할
      theme_minimal() +
      labs(x = x,
           title = "Histogram by Group",
           fill = group_var,  # fill 범례 제목 변경
           color = group_var) +
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


  return(p)

}


p <- ggplot(df, aes(x = x_Var)) +
  geom_histogram(aes(fill = group_var), color = "black", bins = 20, alpha = 0.5) +
  scale_fill_manual(values = colors_hist) +  # 히스토그램의 색상을 지정합니다.
  facet_wrap(~ group_var, scales = "free_y") +  # 각 그룹별로 패널을 분할합니다.
  theme_minimal() +
  labs(x = x,
       title = "Histogram with Density by Group",
       fill = "Group") +  # 히스토그램의 범례 제목을 변경합니다.
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  geom_density(aes(color = group_var), size = 1, alpha = 0.3) +  # density plot을 추가합니다.

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




