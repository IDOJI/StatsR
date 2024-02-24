ggplot___correlation = function(df, x, y, method = "pearson"){
  p <- ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point() +  # 산점도
    geom_smooth(method = "lm", se = TRUE) +  # 선형 회귀선 및 신뢰 구간
    stat_cor(method = method, label.x = 3, label.y = 10) +  # 상관 계수
    theme_minimal() +  # 최소한의 테마
    labs(title = "Scatter plot with linear regression", x = x, y = y) +  # 제목 및 축 레이블 추가
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # 플롯 제목 스타일
      axis.title.x = element_text(size = 14, face = "bold"),  # x축 레이블 스타일
      axis.title.y = element_text(size = 14, face = "bold")   # y축 레이블 스타일
    )

  return(p)
}
