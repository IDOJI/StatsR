ggplot___scatterplot = function(df=NULL, x, y, method = "pearson",...){
  # 🟥 Extract vectors #####################################################
  if(is.null(df)){
    df = data.frame(x=x, y=y)
  }



  # 🟥 Pearson Correlation #####################################################
  R = cor(df[,x], df[,y], method = method) %>% round(., 4)



  # 🟥 Scatter plot #####################################################
  p <- ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point() +  # 산점도
    geom_smooth(method = "lm", se = TRUE) +  # 선형 회귀선 및 신뢰 구간
    # stat_cor(method = method, label.x = 3, label.y = 10) +  # 상관 계수
    theme_minimal() +  # 최소한의 테마
    labs(title = "Scatter plot with linear regression",
         subtitle = paste0("(", method, " correlation = ", R, ")"),
         x = x,
         y = y) +  # 제목 및 축 레이블 추가
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # 플롯 제목 스타일
      plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic"),  # subtitle 스타일 조정
      axis.title.x = element_text(size = 14, face = "bold"),  # x축 레이블 스타일
      axis.title.y = element_text(size = 14, face = "bold")   # y축 레이블 스타일
    )

  return(p)
}
