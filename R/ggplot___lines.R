ggplot___lines = function(df,
                          col_names = NULL,
                          x = NULL,
                          key = "ID",
                          value = "value",
                          xlab = "Distance",
                          ylab = "Functional Connectivity",
                          path_Export = NULL,
                          file.name = NULL,
                          width = 20,
                          height = 5){
  # x
  if(is.null(x)){
    x = 1:nrow(df)
  }



  tidyr::pivot_longer()
  # 🟥 Colnames ====================================================================
  # 🟨 Check input ===============================================================
  if(is.null(col_names)){
    col_names = names(df)
  }


  # 🟨 Subset ===============================================================
  df_selected = df[, col_names]

  matplot(df_selected, type = "l")


  # transform to long-format
  tidyr::pivot_longer(data = df_selected,
                      cols = col_names)

  df_selected



  library(ggplot2)
  library(tidyr)

  # 함수 정의
  ggplot_line <- function(df, col_names = NULL) {
    # col_names가 NULL이 아니면, 해당 열만 선택
    if (!is.null(col_names)) {
      df <- df[, c("x", col_names), drop = FALSE] # x는 x축 변수, 변경 가능
    }

    # 데이터 프레임을 긴 형식으로 변환
    long_df <- pivot_longer(df, cols = -x, names_to = "variable", values_to = "value")

    # ggplot을 이용한 라인 플롯 생성
    ggplot(long_df, aes(x = x, y = value, color = variable)) +
      geom_line() +
      theme_minimal() +
      labs(x = "X Axis", y = "Value", title = "Line Plot") # 레이블 변경 가능
  }

  # 예제 데이터 프레임
  df_example <- data.frame(
    x = 1:10,
    y1 = sin(1:10),
    y2 = cos(1:10)
  )

  # 함수 사용 예시
  # 모든 열을 사용하는 경우
  ggplot_line(df_example)

  # 특정 열만 사용하는 경우
  ggplot_line(df_example, col_names = c("y1", "y2"))




  # 🟥 plotting ====================================================================
  if(length(col_names)==1){

    ggplot___lines___one(df_selected)

  }else{

    ggplot___lines___mult(df_selected)

  }


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

  if(!is.null(path_Export)){
    ggsave(paste0(path_Export, "/", file.name, ".png"), p, bg = "white", width = width, height = height)
  }

  return(p)
}
