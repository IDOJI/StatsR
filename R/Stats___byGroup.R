Stats___byGroup <- function(df, group_var, numeric_var, na.rm=TRUE) {

  # 그룹 별 통계량 계산
  stats <- df %>%
    dplyr::group_by(!!sym(group_var)) %>%
    dplyr::summarise(
      count = n(),
      proportion = n() / nrow(df),
      mean = mean(!!sym(numeric_var), na.rm = na.rm),
      sd = sd(!!sym(numeric_var), na.rm = na.rm)
    ) %>%
    dplyr::ungroup()

  return(stats)
}


