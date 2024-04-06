mean_difference_test <- function(normality_results_list, homogeneity_results, data, paired = FALSE, alpha = 0.05) {
  # 결과를 저장할 리스트 초기화
  results_list <- list()
  # 반응 변수의 개수 확인
  response_vars_count <- length(names(normality_results_list))
  # 보정된 유의수준 계산
  adjusted_alpha <- alpha / response_vars_count

  # P 값에 따른 유의성을 나타내는 함수 정의
  SUB___P.vals.Signif.Stars <- function(p.vals, show.NS = TRUE) {
    # P 값에 따른 유의성 문자열 생성
    if(show.NS) {
      ifelse(p.vals < 0.001, "***",
             ifelse(p.vals <= 0.01, "**",
                    ifelse(p.vals <= 0.05, "*",
                           ifelse(p.vals > 0.1, "HNS", "NS"))))
    } else {
      ifelse(p.vals < 0.001, "***",
             ifelse(p.vals <= 0.01, "**",
                    ifelse(p.vals <= 0.05, "*", "")))
    }
  }

  # 그룹 변수 추출
  group_var <- homogeneity_results[[names(homogeneity_results)[1]]]$DataFrame$Group_Var[1]

  # 반응 변수마다 테스트 수행
  for(response_var in names(normality_results_list)) {
    # 정규성 검정 결과 및 동질성 검정 결과 추출
    normality <- normality_results_list[[response_var]]$is.normal
    homogeneity <- homogeneity_results[[response_var]]$Homogeneity

    # 그룹의 개수 추출
    num_groups <- length(unique(data[[group_var]]))
    # 테스트 이름 및 통계량 초기화
    test_name <- "Not Applicable"
    statistic_value <- NA
    p_value <- NA

    # 각 그룹의 샘플 크기가 30 이상인 경우 정규성 가정 충족
    if(min(table(data[[group_var]])) >= 30) {
      normality <- TRUE
    }



    # 정규성 가정이 충족되고 그룹이 2개인 경우
    if(normality && num_groups == 2) {
      # 등분산 가정에 따라 t-검정 수행
      test_result <- t.test(data[[response_var]] ~ data[[group_var]], var.equal = homogeneity, paired = paired)

      test_name = ifelse(paired, "Paired t-test", "Two sample t-test") %>%
        ifelse(!homogeneity, paste0("Welch's ", .), .)

    } else if(!normality && num_groups == 2) {
      # 정규성 가정이 충족되지 않는 경우 Wilcoxon 부호 순위 검정 수행
      test_result <- wilcox.test(data[[response_var]] ~ data[[group_var]], paired = paired)
      test_name = ifelse(paired, "Wilcoxon signed-rank test", "Mann-Whitney U test")

    } else if(normality && num_groups > 2) {
      # 정규성 가정이 충족되고 그룹이 3개 이상 : ANOVA
      test_result = oneway.test(data[[response_var]] ~ data[[group_var]], data=data, var.equal = homogeneity)
      test_name = ifelse(homogeneity, "One-Way ANOVA", "Welch's ANOVA")

    } else if(!normality && num_groups > 2 && paired) {
      # 정규성 가정이 충족되지 않고, 그룹이 2개 이상이며, 데이터가 순위 데이터이고 반복 측정이 있는 경우 프리드먼 검정 수행
      test_result <- friedman.test(data[[response_var]], data[[group_var]], data[[block_var]])
      test_name = "Friedman"

    } else if(!normality && num_groups > 2) {
      # 정규성 가정이 충족되지 않고 그룹이 2개 이상인 경우 Kruskal-Wallis 검정 수행
      test_result <- kruskal.test(data[[response_var]] ~ data[[group_var]], data = data)
      test_name =  "Kruskal-Wallis test"
    }

    # 결과 처리
    results_list[[response_var]] <- data.frame(
      Response = response_var,
      Test = test_name,
      Statistic = test_result$statistic,
      P_Value = test_result$p.value,
      Adjusted_P_Value = min(1, test_result$p.value * response_vars_count), # 혹은 adjusted_alpha를 사용, 상황에 따라
      Significance = SUB___P.vals.Signif.Stars(min(1, test_result$p.value * response_vars_count), TRUE),
      stringsAsFactors = FALSE
    )
  }

  # 결과 리스트 반환
  return(results_list)
}
