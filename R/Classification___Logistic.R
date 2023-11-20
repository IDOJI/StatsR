Classification___Logistic = function(Logistic){
  #=============================================================================
  # Generated path
  #=============================================================================
  if(!is.null(Logistic$path_Export)){
    fs::dir_create(Logistic$path_Export, recurse = T)
  }






  #=============================================================================
  # Check the data
  #=============================================================================
  # only numeric input
  Logistic$Train_X = apply(Logistic$Train_X, 2, as.numeric) %>% dplyr::as_tibble()
  Logistic$Test_X = apply(Logistic$Test_X, 2, as.numeric) %>% dplyr::as_tibble()



  # standardization for Penalizations
  if(Logistic$Standardize){
    safe_scale <- function(x) {
      if (sd(x) == 0) {
        return(rep(0, length(x)))
      } else {
        return(scale(x))
      }
    }

    Logistic$Train_X = Logistic$Train_X %>% mutate_if(is.numeric, safe_scale)
    Logistic$Test_X = Logistic$Test_X %>% mutate_if(is.numeric, safe_scale)
  }


  # Compare input dimension
  if(!is.null(Logistic$Train_X) && ncol(Logistic$Train_X)!=ncol(Logistic$Test_X)){
    stop("Check input Test_X and Train_X. Its ncols are different!")
  }


  # as.matrix
  Logistic$Train_X = Logistic$Train_X %>% as.matrix
  Logistic$Test_X = Logistic$Test_X %>% as.matrix


  # Response
  Logistic$Train_y = Logistic$Train_y %>% droplevels()
  Logistic$Test_y = Logistic$Test_y %>% droplevels()


  # Penalty factor
  if(is.null(Logistic$penalty_factor)){
    Logistic$penalty_factor = rep(1, ncol(Logistic$Train_X))
  }







  #=============================================================================
  # Fitting Models
  #=============================================================================
  Groups = Logistic$Train_y %>% unlist %>% levels %>% length


  if(Groups == 2){

    Logistic = Classification___Logistic___Binomial(Logistic)

  }else if(Groups > 2){

    Logistic = Classification___Logistic___Multinomial(Logistic)

  }








  #=============================================================================
  # Return
  #=============================================================================
  cat("\n", crayon::green("Fitting a Logistic model is done!"), "\n")
  return(Logistic)
}
#===============================================================================
# Arguments Setting
#===============================================================================s
# Logistic = list(#----------------------------------------
#                 # Data Setting
#                 #----------------------------------------
#                 Train_X = Train_X,
#                 Train_y = Train_y, # factor with levels
#                 Test_X = Test_X,
#                 Test_y = Test_y, # factor with levels
#                 Train_Folds_Index.vec = Train_Folds_Index.vec,
#                 Train_Folds_Index.list = Train_Folds_Index.list,
#                 Standardize = TRUE,
#                 #----------------------------------------
#                 # Modeling Fitting
#                 #----------------------------------------
#                 # Method
#                 Response_Type = c("Nominal", "Ordinal"),
#                 Fitting_Method = c("MLE", "ElasticNet",
#                                    # Grouped Penalty
#                                    "grLasso", "grMCP","grSCAD", "gel", "cMCP"
#                                    ), #
#                 #
#                 Cut_Off = 0.5, # for Binomial prediction cut-off
#                 # Model
#                 Family = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                            "cumulative", "sratio", "cratio", "acat",
#                            #Classification___Logistic___Nominal___Elastic___NonGroupedPenalty : glmnet
#                            "gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian",
#                            # glm인 경우 Link argument 참고
                              # # Group penalty
                              # "gaussian", "binomial", "poisson"
#                            ),
#                 Link = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                          "logit", "probit", "cloglog", "cauchit",
#                          #Classification___Logistic___Ordinal___MLE : method
#                          "logistic", "probit", "loglog", "cloglog", "cauchit",
#                          #Classification___Logistic___Nominal___MLE___NonGroupedPenalty
#                          "logit", "identity", "inverse", "log", "identity", "logit", "log"
#                          ),
#                 # Penalty
#                 penalty_alpha = seq(0, 1, 0.01),
#                 penalty_lambda = exp(seq(-2,2,0.01)),
#                 penalty.factor = rep(1, ncol(Train_X)), # which variables no penalty? The corresponding position of 0 is the variables with no penalty
#                 #----------------------------------------
#                 # Tuning measures
#                 #----------------------------------------
#                 Tune_Method = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                                 "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic",
#                                 #Classification___Logistic___Nominal___Elastic___NonGroupedPenalty
#                                 "default", "mse", "deviance", "class", "auc", "mae", "C"),
#                 # Best_Model_Criterion = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                 #                          "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"),
#                 #----------------------------------------
#                 # Grouping variables
#                 #----------------------------------------
#                 Grouped_Vars_Index = NULL, # NULL이 아니면 그룹 정보를 사용, 그룹 위치 벡터를 넣어야 함.
#                 #----------------------------------------
#                 # Plotting
#                 #----------------------------------------
#                 Plot_y_varname = NULL, # proportional logit plot은 하나의 변수만 가능하므로 한 변수 지정
#                 Plot_x_varname = NULL, # 지정하지 않으면 plot 안 그려짐
#                 AUC_in_Legend = TRUE,
#                 #----------------------------------------
#                 # Export Results
#                 #----------------------------------------
#                 path_Export = NULL)






#
# 결정 트리(Decision Trees) 분류기를 사용하기 위한 입력 형식은 데이터 설정, 트리의 복잡성을 제어하는 옵션, 모델 성능 평가, 결과 출력 등의 영역을 정의해야 합니다. 다음은 결정 트리 분류를 위한 기본 입력 형식의 예시입니다:
#
#   ```r
# DecisionTrees = list(
#   #----------------------------------------
#   # Data Setting
#   #----------------------------------------
#   Train_X = Train_X,                   # Training features
#   Train_y = Train_y,                   # Training labels (factor with levels)
#   Test_X = Test_X,                     # Testing features
#   Test_y = Test_y,                     # Testing labels (factor with levels)
#   #----------------------------------------
#   # Model Settings
#   #----------------------------------------
#   Type = "CART",                       # Tree type, e.g., "CART" or "C4.5"
#   #----------------------------------------
#   # Model Complexity Control
#   #----------------------------------------
#   Max_Depth = NULL,                    # Maximum depth of the tree
#   Min_Split = 2,                       # Minimum number of observations that must exist in a node in order to attempt a split
#   Min_Bucket = 1,                      # Minimum number of observations in any terminal leaf node
#   Complexity_Parameter = 0.01,         # Complexity parameter for pruning
#   #----------------------------------------
#   # Hyperparameter Tuning
#   #----------------------------------------
#   Tune_Hyperparameters = TRUE,         # Whether to tune hyperparameters
#   Hyperparameter_Grid = list(          # Grid of hyperparameters to be tuned
#     max_depth = seq(1, 30, by = 1),
#     min_split = seq(2, 20, by = 1),
#     complexity_parameter = seq(0.001, 0.1, by = 0.001)
#   ),
#   #----------------------------------------
#   # Performance Metrics
#   #----------------------------------------
#   Metrics = c("accuracy", "precision", "recall", "f1"), # Metrics to evaluate model performance
#   #----------------------------------------
#   # Cross-Validation Settings
#   #----------------------------------------
#   Cross_Validation_Folds = 10,         # Number of folds in cross-validation
#   #----------------------------------------
#   # Plotting
#   #----------------------------------------
#   Plot_Tree = TRUE,                    # Whether to plot the tree
#   #----------------------------------------
#   # Export Results
#   #----------------------------------------
#   Export_Results = TRUE,               # Whether to export the results
#   path_Export = "path/to/export/results" # Path to export the results
# )
# ```
#
# 여기서 `Type`은 결정 트리의 유형을 정의하고, `Max_Depth`, `Min_Split`, `Min_Bucket`, `Complexity_Parameter` 등은 트리의 성장과 가지치기에 영향을 주는 모델 복잡성을 제어하는 옵션들입니다. `Tune_Hyperparameters`와 `Hyperparameter_Grid`는 최적의 트리 매개변수를 찾기 위한 옵션을 설정합니다. `Metrics`는 모델의 성능을 평가하는 데 사용되는 지표를 정의하고, `Export_Results` 및 `path_Export`는 모델 결과의 내보내기 여부와 경로를 설정합니다.
#
# 이 템플릿을 기반으로 사용자는 자신의 데이터셋과 분석 목적에 맞게 각 매개변수를 조정하여 결정 트리 모델을 적용할 수 있습니다.
