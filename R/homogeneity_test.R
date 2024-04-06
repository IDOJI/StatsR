homogeneity_test <- function(normality_results_list,
                             data,
                             group_var,
                             alpha = 0.05) {
  homogeneity_results <- list()  # 등분산 검정 결과와 등분산성 여부를 저장할 리스트

  # 등분산성 검정 결과 요약을 위한 데이터 프레임 생성 함수
  summarize_test_result <- function(test_result, group_var_name, response_var_name) {
    return(data.frame(
      Group_Var = group_var_name,  # 그룹 변수 이름
      Response_Var = response_var_name,  # 반응 변수 이름
      Test = deparse(substitute(test_result)),  # 검정 이름
      Statistic = test_result$statistic,  # 통계량
      P_Value = test_result$p.value,  # p값
      stringsAsFactors = FALSE
    ))
  }

  for(response_var in names(normality_results_list)) {
    result <- normality_results_list[[response_var]]
    is_normal <- result$is.normal  # 정규성 검사 결과

    # 등분산성 검정 수행
    if (is_normal) {
      if (length(unique(data[[group_var]])) == 2) {
        test_result <- var.test(data[[response_var]] ~ data[[group_var]], data = data)
      } else {
        test_result <- bartlett.test(data[[response_var]] ~ data[[group_var]], data = data)
      }
    } else {
      if (any(result$DataFrame$TestMethod == "MJBTest")) {
        test_result <- fligner.test(data[[response_var]] ~ data[[group_var]], data = data)
      } else {
        # car 패키지의 leveneTest 함수 사용
        if (!requireNamespace("car", quietly = TRUE)) {
          stop("The 'car' package is required for leveneTest but is not installed.")
        }
        test_result <- car::leveneTest(data[[response_var]] ~ data[[group_var]], data = data)
      }
    }

    # 등분산성 검정 결과 요약
    test_summary <- summarize_test_result(test_result, group_var, response_var)

    # 등분산성 여부 판단
    homogeneity <- test_result$p.value >= alpha

    # 결과 저장
    homogeneity_results[[response_var]] <- list(
      DataFrame = test_summary,
      Homogeneity = homogeneity
    )
  }

  return(homogeneity_results)
}
