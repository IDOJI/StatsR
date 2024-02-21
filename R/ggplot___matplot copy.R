ggplot_matplot = function(df, x = NULL,  key = "ID", value = "value", xlab = "Distance", ylab = "Functional Connectivity", path_Export, file.name, width = 20, height = 5){
  # x
  if(is.null(x)){
    x = 1:nrow(df)
  }
  # lengthen data.frame
  df_long = cbind(x, df) %>%
    gather(key = key, value = value, -x)


  p <- ggplot() +
    geom_line(data = df_long, aes(x = x, y = value, group = key, color = key), show.legend = FALSE) +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),  # x축 레이블 크기와 색상 설정
      axis.title.y = element_text(size = 20, face = "bold"),  # y축 레이블 크기와 색상 설정
      plot.title = element_text(size = 30, color = "black", hjust = 0.5, face = "bold")  # 그래프 제목 크기, 색상 및 위치 설정

    )

  # path_Export = "C:/Users/lleii/Dropbox/GitHub/GitHub___Papers/GitHub___Papers___Writing/GitHub___Papers___Writing___MS/PPT/Figure_FC curves"
  # file.name = "FC Curves"

  ggsave(paste0(path_Export, "/", file.name, ".png"), p, bg = "white", width = width, height = height)



}
