Stats___DF <- function(df, selected_columns=NULL) {
  # 선택한 열만 추출
  if(!is.null(selected_columns)){
    df_selected <- df[, selected_columns, drop = FALSE]
  }else{
    df_selected = df
  }


  stats <- lapply(df_selected, function(col) {
    if (is.factor(col) || is.character(col)) {
      # 카테고리형 데이터인 경우 비율과 퍼센티지 계산
      counts <- table(col)
      proportions <- prop.table(counts)
      percentages <- proportions * 100
      result <- data.frame(counts = counts, proportions = proportions, percentages = percentages)
    } else if (is.numeric(col)) {
      # 숫자형 데이터인 경우 평균과 표준편차 계산
      mean_val <- mean(col, na.rm = TRUE)
      sd_val <- sd(col, na.rm = TRUE)
      result <- data.frame(mean = mean_val, sd = sd_val)
    } else {
      # 그 외의 경우에는 NULL 반환
      result <- NULL
    }
    return(result)
  })

  # 결과를 데이터프레임으로 변환하여 반환
  stats_df <- do.call(cbind, stats)
  return(stats_df)
}
