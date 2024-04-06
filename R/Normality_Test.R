Normality_test <- function(data, group_var, response_vars, alpha = 0.05) {
  final_results_list <- list()  # 최종 결과를 저장할 빈 리스트를 생성합니다.

  if(is.null(response_vars)) {  # 만약 response_vars가 NULL이라면,
    response_vars <- setdiff(names(data), group_var)  # 그룹 변수를 제외한 모든 변수를 response_vars로 설정합니다.
  }

  grouped_data <- split(data, data[,group_var])  # 데이터를 그룹화합니다.

  for(response_var in response_vars) {  # 각 response 변수에 대해 반복합니다.

    results_df <- data.frame()  # 결과를 저장할 빈 데이터프레임을 생성합니다.
    is_normal <- TRUE  # 정규성을 나타내는 변수를 초기화합니다.

    for(group_name in names(grouped_data)) {  # 각 그룹에 대해 반복합니다.
      group_data <- grouped_data[[group_name]]  # 현재 그룹의 데이터를 선택합니다.
      if(!response_var %in% names(group_data)) {  # 만약 현재 그룹에 response 변수가 없다면,
        next  # 다음 반복을 진행합니다.
      }

      response_data <- group_data[[response_var]]  # 현재 response 변수에 해당하는 데이터를 선택합니다.

      # 아웃라이어 검출
      Q1 <- quantile(response_data, 0.25, na.rm = TRUE)  # 1사분위수를 계산합니다.
      Q3 <- quantile(response_data, 0.75, na.rm = TRUE)  # 3사분위수를 계산합니다.
      IQR <- Q3 - Q1  # IQR을 계산합니다.
      outlier_exists <- any(response_data < (Q1 - 1.5 * IQR) | response_data > (Q3 + 1.5 * IQR), na.rm = TRUE)  # 이상치가 있는지 확인합니다.

      # 아웃라이어 검출 및 정규성 검정 수행 부분
      test_result <- if(length(na.omit(response_data)) > 4) {
        if(outlier_exists) {
          # MJBTest 함수 사용
          MJBTest_result <- MJBTest(response_data)
          # MJBTest 결과를 바탕으로 리스트 생성
          list(statistic = MJBTest_result$MJB_statistic, p.value = MJBTest_result$p_value)
        } else {
          # shapiro.test 사용
          shapiro_result <- shapiro.test(response_data)
          # shapiro.test 결과를 바탕으로 리스트 생성
          list(statistic = shapiro_result$statistic, p.value = shapiro_result$p.value)
        }
      } else {
        # 데이터 포인트가 충분하지 않음
        list(statistic=NA, p.value=NA)
      }


      # 그룹별 관측값의 수를 계산합니다.
      group_size <- sum(!is.na(group_data[[response_var]]))


      # 결과 데이터 프레임에 추가하는 부분
      temp_df <- data.frame(
        Response = response_var,
        Group_Var = group_var,
        Group = group_name,
        N = group_size,  # 각 그룹별 관측값의 수를 추가합니다.
        TestMethod = if(outlier_exists) "MJBTest" else "Shapiro-Wilk",
        Statistic = test_result$statistic,
        P_Value = test_result$p.value,
        stringsAsFactors = FALSE
      )


      # 데이터 프레임 생성
      temp_df <- data.frame(
        Response = response_var,
        Group_Var = group_var,
        Group = group_name,
        N = group_size,
        TestMethod = if(outlier_exists) "MJBTest" else "Shapiro-Wilk",
        Statistic = test_result$statistic,
        P_Value = test_result$p.value,
        stringsAsFactors = FALSE
      )


      results_df <- rbind(results_df, temp_df)  # 결과를 결과 데이터프레임에 추가합니다.

    }

    results_df[-1,1:2] = NA


    # 그룹의 수에 따라 alpha 값을 조정합니다.
    adjusted_alpha <- alpha / length(unique(results_df$Group))

    # 유의성 결과를 계산하고 결과 데이터 프레임에 추가합니다.
    results_df$Significance <- ifelse(results_df$P_Value < adjusted_alpha, "***",
                                      ifelse(results_df$P_Value < (alpha / 2), "**",
                                             ifelse(results_df$P_Value < alpha, "*", "NS")))


    # 모든 그룹에서 정규분포 가정이 성립하는지 확인합니다.
    is_normal <- all(results_df$Significance == "NS")

    # 결과 데이터 프레임과 is_normal 값을 최종 결과 리스트에 추가합니다.
    final_results_list[[response_var]] <- list(DataFrame = results_df, is.normal = is_normal)
  }



  return(final_results_list)  # 최종 결과 리스트를 반환합니다.
}
