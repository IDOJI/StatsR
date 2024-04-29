ggplot___boxplot <- function(x) {
  # 벡터 x를 데이터 프레임으로 변환
  data <- data.frame(Value = x)

  # ggplot을 사용하여 박스플롯 생성
  p <- ggplot(data, aes(y = Value)) +
    geom_boxplot(fill = "lightblue", colour = "darkblue", outlier.colour = "red", outlier.shape = 2, outlier.size = 4, outlier.stroke = 2) +
    labs(y = "Values", title = "Boxplot of Vector Values with Distinct Outliers") +
    theme_light(base_size = 14) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    ylim(min(x) - IQR(x) * 1.5, max(x) + IQR(x) * 1.5)  # Y축 범위 조정

  # 박스플롯 출력
  print(p)
}
