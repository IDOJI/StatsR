Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty = function(Logistic){
  #=============================================================================
  # install.packages
  #=============================================================================
  install_packages(packages = c("ordinalNet", "fs"), load = T)







  #=============================================================================
  # Input argument
  #=============================================================================
  # Family
  Logistic$Family = match.arg(Logistic$Family, c("cumulative", "sratio", "cratio", "acat"))


  # Link
  Logistic$Link = match.arg(Logistic$Link, c("logit", "probit", "cloglog", "cauchit"))


  # Tuning Method
  Logistic$Tune_Method = match.arg(Logistic$Tune_Method, c("cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"))








  #=============================================================================
  # Fitting by CV
  #=============================================================================
  Fit_CV.list = lapply(Logistic$penalty_alpha, function(ith_alpha, ...){

    tictoc::tic()
    # Fitting CV for ith_alpha

    ij_CV_Result = lapply(Logistic$penalty_lambda, function(jth_lambda){
      ith_Fit_CV = ordinalNet::ordinalNetCV(x = Logistic$Train_X %>% as.matrix,
                                            y = Logistic$Train_y %>% unlist,
                                            standardize = FALSE,
                                            family = Logistic$Family,
                                            link = Logistic$Link,
                                            tuneMethod = Logistic$Tune_Method,
                                            lambdaVals = jth_lambda,
                                            alpha = ith_alpha,
                                            fold = Logistic$Train_Folds_Index.list, # "list" with legnth of n_folds : Fold_1 should include row indice for Fold_1
                                            printProgress = TRUE,
                                            warn = TRUE)
    })

    #  Save result for each alpha
    saveRDS(ith_Fit_CV, file = paste0(Logistic$path_Export, "/", "Fit_CV", "___", ith_alpha, ".rds"))
    tictoc::toc()

    # Averaging
    ith_Fit_CV_Mean = summary(ith_Fit_CV) %>% colMeans()
    return(ith_Fit_CV_Mean)
  })







  #=============================================================================
  # When error happens, we need to load the saved fitting results
  #=============================================================================
  Fit_CV.list = lapply(list.files(Logistic$path_Export, full.names=T, pattern = "Fit_CV_"), function(y){
    readRDS(y) %>% summary %>% colMeans
  })







  #=============================================================================
  # Select best parameters
  #=============================================================================
  # best.model.criterion = match.arg(best.model.criterion)
  if(Logistic$Tune_Method %in% c("cvMisclass", "cvBrier")){
    which_best = which.min
  }else{
    which_best = which.max
  }


  Combined_Criteria = sapply(Fit_CV.list, function(ith_Fit_CV){
    if(Logistic$Tune_Method == "cvLoglik"){

     ith_Fit_CV[names(ith_Fit_CV)=="loglik"] %>% return

    }else if(Logistic$Tune_Method == "cvMisclass"){

      ith_Fit_CV[names(ith_Fit_CV)=="misclass"] %>% return

    }else if(Logistic$Tune_Method == "cvBrier"){

      ith_Fit_CV[names(ith_Fit_CV)=="brier"] %>% return

    }else if(Logistic$Tune_Method == "cvDevPct"){

      ith_Fit_CV[names(ith_Fit_CV)=="devPct"] %>% return

    }
  })

  Best_ind = which_best(Combined_Criteria)
  Best_alpha = Logistic$penalty_alpha[Best_ind]
  # best_lambda = Fit_CV.list[[best_ind]][1]









  #=============================================================================
  # Fit again with best parameters
  #=============================================================================
  Best_Fit = ordinalNet(x = Logistic$Train_X %>% as.matrix,
                        y = Logistic$Train_y %>% unlist,
                        alpha = Best_alpha,
                        standardize = FALSE,
                        family = Logistic$Family,
                        link = Logistic$Link)










  #=============================================================================
  # Inserting best
  #=============================================================================
  # if(which.min(Best_Fit_Summary$aic) == which.min(Best_Fit_Summary$bic)){
  #   best_lambda = Best_Fit$lambdaVals[which.min(Best_Fit_Summary$aic)]
  # }else{
  #   stop("Best indice of AIC and BIC are different!")
  # }
  # Results summary
  Best_Fit_Summary = summary(Best_Fit)


  # 초기 사용자 입력 변수 선언
  user_input_n = ""

  while(TRUE) {
    print(Best_Fit_Summary)

    # 사용자 입력 받기
    user_input_n <- readline(prompt="Which model is best? Input the number of row: ")

    # 입력값이 숫자로만 구성되었는지 확인
    if(grepl("^[0-9]+$", user_input_n)) {
      line_number <- as.numeric(user_input_n)

      # 입력된 값이 유효한 행 번호인지 확인
      if(line_number > 0 && line_number <= nrow(Best_Fit_Summary)) {

        Best_lambda = Best_Fit_Summary[line_number, "lambdaVals"]

        print(paste("The selected best lambda is:", Best_lambda))
        break # 유효한 입력이므로 while 루프 종료
      } else {
        print("잘못된 라인 번호입니다. 다시 입력해주세요.")
      }
    } else {
      print("유효한 숫자를 입력해주세요.")
    }
  }










  #=============================================================================
  # Fit again using best parameters
  #=============================================================================
  Best_Fit_Final = ordinalNet(x = Logistic$Train_X %>% as.matrix,,
                              y = Logistic$Train_y %>% unlist,
                              lambdaVals = Best_lambda,
                              alpha = Best_alpha,
                              standardize = FALSE,
                              family = Logistic$Family,
                              link = Logistic$Link)









  #=============================================================================
  # Combine Resulst
  #=============================================================================
  Logistic$Best_Model = Best_Fit_Final
  Logistic$Best_alpha = Best_alpha







  #=============================================================================
  # Extract results and prediction & Exporting
  #=============================================================================
  Results.list = Classification___Logistic___Results(Logistic)


  return(Results.list)
}











