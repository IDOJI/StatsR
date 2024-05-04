sub___model.performance___classification <- function(y_true, y_pred) {
  # Input 검사 =======================================================
  if (!is.numeric(y_true) || !is.numeric(y_pred)) {
    stop("The input should be numeric class!")
  }
  if (length(y_true) != length(y_pred)) {
    stop("y_true and y_pred must have the same length!")
  }
  if (!all(y_true %in% c(0, 1)) || !all(y_pred %in% c(0, 1))) {
    stop("The input should 1 (positive) or 0 (negative) as numeric!")
  }

  # 혼동 행렬 생성 ==========================================================
  # 혼동 행렬의 인덱싱 오류:
  # confusion_matrix를 table 함수로 생성할 때, 
  # y_true와 y_pred의 값에 따라 행과 열의 순서가 예상과 다를 수 있습니다. 
  # 만약 예측값이나 실제값 중 하나가 0 또는 1만 포함되어 있다면, 
  # confusion_matrix는 단일 차원으로 반환될 수 있고, 
  confusion_matrix <- table(factor(y_pred, levels = c(0, 1)), factor(y_true, levels = c(0, 1)))

  # 각 테이블 값들 ============================================================
  TN = confusion_matrix[1, 1]
  TP = confusion_matrix[2, 2]
  FN = confusion_matrix[1, 2]
  FP = confusion_matrix[2, 1]

  # 성능 지표 계산 ==================================================================
  accuracy = (TP + TN) / sum(confusion_matrix)
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  specificity = TN / (TN + FP)
  F1_score = 2 * (precision * recall) / (precision + recall)

  # 결과 반환 ===========================================================
  list(accuracy = accuracy,
       precision = precision,
       recall = recall,
       specificity = specificity,
       F1_score = F1_score)
}
