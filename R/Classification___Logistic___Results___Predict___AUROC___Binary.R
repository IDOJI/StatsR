Classification___Logistic___Results___Predict___AUROC___Binary = function(Predicted_Probs, y_Test_unlist, Categories){
  # 예측된 확률에서 범주의 확률을 선택합니다.
  # 여기서는 두 번째 열을 AD로 가정합니다.
  Predicted_Probs_AD <- Predicted_Probs[, 2]

  # 이진 레이블을 생성합니다.
  binary_labels <- ifelse(y_Test_unlist == "AD", 1, 0)

  # ROC 곡선을 계산합니다.
  roc_curve <- roc(binary_labels, Predicted_Probs_AD)

  # AUC 값을 계산합니다.
  auc_value <- auc(roc_curve)

  # ROC 곡선을 시각화합니다.
  p <- ggplot(data.frame(FPR = 1 - roc_curve$specificities, TPR = roc_curve$sensitivities),
              aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    annotate("text", x = 0.6, y = 0.4, label = sprintf("AUC: %.3f", auc_value), parse = TRUE) +
    labs(x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal()


  list(p) %>% return()
}
