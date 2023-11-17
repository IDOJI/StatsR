Classification___Logistic___Binomial___Penalty___Grouped = function(Logistic){
  #=============================================================================
  # Load the glmnet
  #=============================================================================
  install_packages(c("grpreg", "pROC"))





  #=============================================================================
  # CV glmnet
  #=============================================================================
  grpreg::cv.grpreg

  # 필요한 패키지를 설치하고 라이브러리를 불러옵니다.
  if (!require(grpreg)) install.packages("")
  library(grpreg)

  # 예시 데이터 생성
  set.seed(123)
  n <- 100 # 관측치 개수
  p <- 10  # 변수 개수
  groups <- rep(1:2, each = p/2) # 변수를 두 그룹으로 나눕니다.
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  beta <- c(rep(1, 5), rep(0, 5)) # 첫 번째 그룹에만 계수를 부여합니다.
  y <- rbinom(n, 1, prob = plogis(X %*% beta))

  # 그룹 라쏘 로지스틱 회귀 적합
  fit <- grpreg(X, y, group = groups, family = "binomial")

  # 모델 요약
  summary(fit)

  # 최적의 람다 값 선택 (예를 들어, CV를 사용)
  cv_fit <- cv.grpreg(X, y, group = groups, family = "binomial")
  best_lambda <- cv_fit$lambda.min

  # 최적의 람다 값으로 적합된 모델 사용
  coef(cv_fit, s = best_lambda)






  #=============================================================================
  # CV glmnet : find optimal lambda
  #=============================================================================







  #=============================================================================
  # Fit best model
  #=============================================================================









  #=============================================================================
  # Extract Results
  #=============================================================================
  Logistic = Classification___Logistic___Results(Logistic)


  return(Logistic)
}








