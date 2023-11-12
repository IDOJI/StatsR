##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
# path_OS = "/Users/Ido/"
# path_External.Drive = "/Volumes/Seagate/"
#============================================================================================
# Windows
#============================================================================================
# path_OS = "C:/Users/lleii/"
#============================================================================================
# require(tidyverse)
# require(dplyr)
# require(pROC)
# require(ggplot2)
# require(clipr)
# require(ggstatsplot)
# list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
# list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
# list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
Classification = function(Logistic = NULL,
                          Bayesian = NULL,
                          SVM = NULL,
                          DecisionTrees = NULL,
                          RandomForest = NULL,
                          ANN = NULL){
  #=============================================================================
  # Logistic Regression
  #=============================================================================
  if(!is.null(Logistic)){
    Results = Classification___Logistic(Logistic)
  }







  #=============================================================================
  # Bayesian classification
  #=============================================================================
  if(!is.null(Bayesian)){
    Results = Classification___Bayesian(Bayesian)
  }







  #=============================================================================
  # SVM
  #=============================================================================
  if(!is.null(SVM)){
    Results = Classification___SVM(SVM)
  }





  #=============================================================================
  # DecisionTrees
  #=============================================================================
  if(!is.null(DecisionTrees)){
    Results = Classification___DecisionTrees(DecisionTrees)
  }




  #=============================================================================
  # RandomForest
  #=============================================================================
  if(!is.null(RandomForest)){

  Results = Classification___RandomForest(DecisionTrees)

  }






  #=============================================================================
  # ANN
  #=============================================================================
  if(!is.null(ANN)){

  Results = Classification___ANN(DecisionTrees)

  }





  cat("\n", crayon::green("Congratulation! The fitting is done!"),"\n")
  return(Results)
}

#=============================================================================
# Example data : Oridnal classfication
#=============================================================================
# # Install and load the mlbench package if not already installed
# if (!require(mlbench)) install.packages("mlbench")
# library(mlbench)
#
# # Load the dataset
# data("PimaIndiansDiabetes", package = "mlbench")
#
# # Check the structure of the dataset
# str(PimaIndiansDiabetes)
#
# Example_Data = PimaIndiansDiabetes
# Example_Data$age_group <- cut(
#   PimaIndiansDiabetes$age,
#   breaks = c(-Inf, 30, 60, Inf),
#   labels = c("young", "middle-aged", "senior"),
#   ordered_result = TRUE
# )
# Example_Data = Example_Data %>% dplyr::select(-c(diabetes, age)) %>% as_tibble
#
# Splitted = Split.Data___CV.Fold___Stratified(Data = Example_Data, Var_1 = "age_group", n_fold = 6)
#
# Train_X = Splitted$Train %>% dplyr::select(-age_group, -Which_Fold)
# Train_y = Splitted$Train %>% dplyr::select(age_group) %>% unlist() %>% unname() %>% factor(levels = c("young", "middle-aged", "senior"))
# Train_Folds_Index.vec = Splitted$Train %>% dplyr::select(Which_Fold)
# Train_Folds_Index.list = Splitted$Train_Fold_Index
#
# Test_X = Splitted$Test %>% dplyr::select(-age_group)
# Test_y = Splitted$Test %>% dplyr::select(age_group) %>% unlist() %>% unname() %>% factor(levels = c("young", "middle-aged", "senior"))























# Bayesian 분류를 사용하기 위해서는 함수 `Classification`에 `Bayesian` 인수로 적절한 입력값을 제공해야 합니다. `Bayesian` 인수는 함수 `Classification___Bayesian`에 필요한 매개변수들을 포함하는 리스트 형태로 구성되어야 합니다. 아래는 Bayesian 분류를 위한 기본 입력 형식의 예시입니다:
#
# ```r
# Bayesian = list(
#   #----------------------------------------
#   # Data Setting
#   #----------------------------------------
#   Train_X = Train_X,                # Training features
#   Train_y = Train_y,                # Training labels (factor with levels)
#   Test_X = Test_X,                  # Testing features
#   Test_y = Test_y,                  # Testing labels (factor with levels)
#   Prior = c("uniform", "jeffreys"), # Prior distribution to use
#   #----------------------------------------
#   # Model Settings
#   #----------------------------------------
#   Model_Type = c("naive_bayes", "bernoulli", "multinomial"), # Type of Bayesian model
#   Use_Kernel = FALSE,               # Use a kernel estimation in naive Bayes
#   Kernel_Type = c("gaussian", "box"), # Type of kernel if kernel estimation is used
#   #----------------------------------------
#   # Model Complexity Control
#   #----------------------------------------
#   Feature_Selection = TRUE,         # Whether to perform feature selection
#   Selection_Criteria = c("AIC", "BIC", "DIC"), # Criteria for feature selection
#   #----------------------------------------
#   # Hyperparameter Tuning
#   #----------------------------------------
#   Tune_Hyperparameters = TRUE,      # Whether to tune hyperparameters
#   Hyperparameter_Grid = list(       # Grid of hyperparameters to be tuned
#     smoothing = seq(0, 1, 0.1)
#   ),
#   #----------------------------------------
#   # Performance Metrics
#   #----------------------------------------
#   Metrics = c("accuracy", "precision", "recall", "f1"), # Metrics to evaluate model performance
#   #----------------------------------------
#   # Plotting
#   #----------------------------------------
#   Plot_ROC = TRUE,                  # Whether to plot ROC curve
#   Plot_ConfusionMatrix = TRUE,      # Whether to plot confusion matrix
#   #----------------------------------------
#   # Export Results
#   #----------------------------------------
#   Export_Results = TRUE,            # Whether to export the results
#   path_Export = "path/to/export/results" # Path to export the results
# )
# ```

# 이 형식을 기반으로 사용자는 자신의 데이터와 모델링 요구에 맞게 각 매개변수를 조정할 수 있습니다.
# 예를 들어, `Prior`은 사용할 사전 분포를 정의하고,
# `Model_Type`은 사용할 Bayesian 모델의 유형을 결정합니다.
# `Feature_Selection`과 `Tune_Hyperparameters`는 모델의 복잡성을 제어하고
# 최적의 매개변수를 찾는 데 사용됩니다.
# 또한, `Metrics`는 모델의 성능을 평가하는 데 사용할 메트릭을 정의합니다.


















#
# SVM (Support Vector Machine) 분류기를 사용하기 위한 입력 형식은 데이터 설정, 모델 선택, 매개변수 튜닝, 성능 메트릭, 결과 출력 등의 영역을 명확히 정의해야 합니다. 아래는 SVM 분류를 위한 기본 입력 형식의 예시입니다:
#
#   ```r
# SVM = list(
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
#   Kernel = c("linear", "radial", "polynomial", "sigmoid"), # Kernel type
#   Cost = 1,                             # Cost parameter (C)
#   Gamma = 1 / ncol(Train_X),            # Kernel coefficient for ‘radial’, ‘polynomial’ and ‘sigmoid’
#   Degree = 3,                           # Degree of the polynomial kernel function
#   Coef0 = 0,                            # Independent term in kernel function
#   #----------------------------------------
#   # Model Complexity Control
#   #----------------------------------------
#   Class_Weights = NULL,                 # Weights for the classes, if imbalance is present
#   #----------------------------------------
#   # Hyperparameter Tuning
#   #----------------------------------------
#   Tune_Hyperparameters = TRUE,          # Whether to tune hyperparameters
#   Hyperparameter_Grid = list(           # Grid of hyperparameters to be tuned
#     cost = 10^seq(-3, 2, by = 1),
#     gamma = 2^seq(-3, 2, by = 1)
#   ),
#   #----------------------------------------
#   # Performance Metrics
#   #----------------------------------------
#   Metrics = c("accuracy", "precision", "recall", "f1"), # Metrics to evaluate model performance
#   #----------------------------------------
#   # Cross-Validation Settings
#   #----------------------------------------
#   Cross_Validation_Folds = 10,          # Number of folds in cross-validation
#   #----------------------------------------
#   # Plotting
#   #----------------------------------------
#   Plot_Support_Vectors = TRUE,          # Whether to plot support vectors
#   #----------------------------------------
#   # Export Results
#   #----------------------------------------
#   Export_Results = TRUE,                # Whether to export the results
#   path_Export = "path/to/export/results" # Path to export the results
# )
# ```
#
# 이 예시에서 `Kernel`은 SVM에서 사용할 커널 유형을, `Cost`와 `Gamma`는 각각 비용 매개변수와 커널 계수를 정의합니다. `Degree`와 `Coef0`는 다항 커널을 사용할 때 필요한 매개변수입니다. `Tune_Hyperparameters` 및 `Hyperparameter_Grid`를 통해 최적의 매개변수 조합을 찾기 위한 그리드 탐색을 설정할 수 있습니다. `Metrics`는 모델 성능을 평가하는 데 사용될 메트릭을 정의하고, `Export_Results`와 `path_Export`는 모델 결과를 내보낼 지 여부와 경로를 설정합니다.
#
# 사용자는 이 기본 템플릿을 자신의 데이터와 요구 사항에 맞게 조정하여 SVM 분류기를 적용할 수 있습니다.
















#
#
# 인공 신경망(ANN) 분류기를 설정하기 위한 입력 형식은 데이터 준비, 신경망 구조와 활성화 함수, 학습 알고리즘 선택, 하이퍼파라미터 튜닝, 성능 평가 지표, 그리고 결과 출력 설정을 포함해야 합니다. 다음은 ANN 분류를 위한 기본 입력 형식의 예시입니다:
#
#   ```r
# ANN = list(
#   #----------------------------------------
#   # Data Setting
#   #----------------------------------------
#   Train_X = Train_X,                   # Training features
#   Train_y = Train_y,                   # Training labels (factor with levels)
#   Test_X = Test_X,                     # Testing features
#   Test_y = Test_y,                     # Testing labels (factor with levels)
#   #----------------------------------------
#   # Network Architecture
#   #----------------------------------------
#   Num_Hidden_Layers = 1,               # Number of hidden layers
#   Units_Per_Layer = c(10),             # Number of units in each hidden layer
#   Activation_Function = "relu",        # Activation function for hidden layers
#   Output_Activation = "softmax",       # Activation function for output layer
#   #----------------------------------------
#   # Training Algorithm
#   #----------------------------------------
#   Optimizer = "adam",                  # Optimization algorithm
#   Loss = "categorical_crossentropy",   # Loss function
#   Epochs = 100,                        # Number of epochs for training
#   Batch_Size = 32,                     # Batch size for training
#   #----------------------------------------
#   # Model Complexity Control
#   #----------------------------------------
#   Dropout_Rate = 0.5,                  # Dropout rate for regularization
#   L1_Regularization = 0.01,            # L1 regularization factor
#   L2_Regularization = 0.01,            # L2 regularization factor
#   #----------------------------------------
#   # Hyperparameter Tuning
#   #----------------------------------------
#   Tune_Hyperparameters = TRUE,         # Whether to tune hyperparameters
#   Hyperparameter_Grid = list(          # Grid of hyperparameters to be tuned
#     units_per_layer = c(5, 10, 15),
#     dropout_rate = c(0.3, 0.5, 0.7),
#     l1_reg = 10^seq(-3, -1, by = 1),
#     l2_reg = 10^seq(-3, -1, by = 1)
#   ),
#   #----------------------------------------
#   # Performance Metrics
#   #----------------------------------------
#   Metrics = c("accuracy"),             # Metrics to evaluate model performance
#   #----------------------------------------
#   # Validation Settings
#   #----------------------------------------
#   Validation_Split = 0.2,              # Fraction of the data to use as validation set
#   #----------------------------------------
#   # Plotting
#   #----------------------------------------
#   Plot_Training_History = TRUE,        # Whether to plot training history
#   #----------------------------------------
#   # Export Results
#   #----------------------------------------
#   Export_Results = TRUE,               # Whether to export the results
#   path_Export = "path/to/export/results" # Path to export the results
# )
# ```
#
# 이 템플릿에서 `Num_Hidden_Layers`와 `Units_Per_Layer`는 각각 숨겨진 계층의 수와 각 계층의 유닛 수를 설정합니다. `Activation_Function`과 `Output_Activation`은 각각 숨겨진 계층과 출력 계층의 활성화 함수를 정의합니다. `Optimizer`와 `Loss`는 훈련 알고리즘과 손실 함수를 지정하며, `Epochs`와 `Batch_Size`는 훈련의 에포크 수와 배치 크기를 결정합니다.
#
# `Dropout_Rate`, `L1_Regularization`, `L2_Regularization`은 과적합을 방지하기 위한 정규화 기법을 설정하고, `Tune_Hyperparameters`와 `Hyperparameter_Grid`는 그리드 탐색을 통한 하이퍼파라미터 튜닝 옵션을 제공합니다. `Metrics`는 모델 성능을 평가할 때 사용되는 지표를, `Export_Results`와 `path_Export`는 결과의 내보내기 여부와 경로를 설정합니다.
#
# 이 기본 형식은 사용자가 자신의 데이터와 분석 목적에 맞게 각 매개변수를 조정하여 인공
#
# 신경망 모델을 구성하는 데 사용할 수 있습니다.


















#
# Random Forest 분류기를 설정하기 위한 입력 형식은 데이터 준비, 모델의 트리 수, 트리의 복잡성을 제어하는 옵션, 하이퍼파라미터 튜닝, 성능 평가 지표, 결과 출력 설정 등을 포함해야 합니다. 다음은 Random Forest 분류를 위한 기본 입력 형식의 예시입니다:
#
#   ```r
# RandomForest = list(
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
#   Num_Trees = 500,                     # Number of trees in the forest
#   Num_Variables_Per_Split = sqrt(ncol(Train_X)), # Number of variables considered for each split
#   #----------------------------------------
#   # Model Complexity Control
#   #----------------------------------------
#   Max_Depth = NULL,                    # Maximum depth of the trees
#   Min_Samples_Split = 2,               # Minimum number of samples required to split a node
#   Min_Samples_Leaf = 1,                # Minimum number of samples required to be at a leaf node
#   #----------------------------------------
#   # Hyperparameter Tuning
#   #----------------------------------------
#   Tune_Hyperparameters = TRUE,         # Whether to tune hyperparameters
#   Hyperparameter_Grid = list(          # Grid of hyperparameters to be tuned
#     num_trees = c(100, 500, 1000),
#     num_variables_per_split = c(sqrt(ncol(Train_X)), ncol(Train_X)/3, ncol(Train_X)/2),
#     max_depth = c(10, 20, 30),
#     min_samples_split = c(2, 4, 6),
#     min_samples_leaf = c(1, 2, 4)
#   ),
#   #----------------------------------------
#   # Performance Metrics
#   #----------------------------------------
#   Metrics = c("accuracy", "precision", "recall", "f1"), # Metrics to evaluate model performance
#   #----------------------------------------
#   # Cross-Validation Settings
#   #----------------------------------------
#   Cross_Validation_Folds = 5,          # Number of folds in cross-validation
#   #----------------------------------------
#   # Plotting
#   #----------------------------------------
#   Plot_Importance = TRUE,              # Whether to plot variable importance
#   Plot_Confusion_Matrix = TRUE,        # Whether to plot confusion matrix
#   #----------------------------------------
#   # Export Results
#   #----------------------------------------
#   Export_Results = TRUE,               # Whether to export the results
#   path_Export = "path/to/export/results" # Path to export the results
# )
# ```
#
# 이 템플릿에서 `Num_Trees`는 숲을 구성하는 트리의 수를 정의하고, `Num_Variables_Per_Split`는 각 분할에서 고려할 변수의 수를 설정합니다. `Max_Depth`, `Min_Samples_Split`, `Min_Samples_Leaf`는 각 트리의 성장을 제한하는 복잡성을 제어하는 옵션들입니다.
#
# `Tune_Hyperparameters`와 `Hyperparameter_Grid`는 최적의 하이퍼파라미터를 찾기 위한 그리드 탐색 옵션을 제공하고, `Metrics`는 모델의 성능을 평가하는 데 사용되는 지표를 정의합니다. `Cross_Validation_Folds`는 교차 검증에 사용될 폴드의 수를 설정합니다.
#
# `Plot_Importance`와 `Plot_Confusion_Matrix`는 변수 중요도와 혼동 행렬을 시각화하는 옵션을 제공하고, `Export_Results` 및 `path_Export`는 결과를 내보낼 지 여부와 내보낼 경로를 설정합니다.
#
# 이 기본 형식은 사용자가 자신의 데이터와 분석 목적에 맞게 각 매개변수를 조정하여 랜덤 포레스트 모델을 구성하는 데 사용할 수 있습니다.





