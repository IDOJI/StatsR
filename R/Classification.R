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
# Logistic Regression
#=============================================================================
# Logistic = list(#----------------------------------------
#                 # Data Setting
#                 #----------------------------------------
#                 Train_X = Train_X,
#                 Train_y = Train_y,
#                 Test_X = Test_X,
#                 Test_y = Test_y,
#                 Train_Folds_Index.vec = Train_Folds_Index.vec,
#                 Train_Folds_Index.list = Train_Folds_Index.list,
#                 Standardize = TRUE,
#                 #----------------------------------------
#                 # Modeling Fitting
#                 #----------------------------------------
#                 # Method
#                 Response_Type = c("Nominal", "Ordinal"),
#                 Fitting_Method = c("MLE", "ElasticNet"), #
#                 # Model
#                 Family = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                            "cumulative", "sratio", "cratio", "acat",
#                            #Classification___Logistic___Nominal___Elastic___NonGroupedPenalty
#                            "gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian"),
#                 Link = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
#                          "logit", "probit", "cloglog", "cauchit"),
#                 # Penalty
#                 penalty_alpha = seq(0, 1, 0.01),
#                 penalty_lambda = exp(seq(-2,2,0.01)),
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
