Classification = function(Logistic = NULL,
                          Bayesian = NULL,
                          SVM = NULL,
                          DecisionTrees = NULL){
  if(!is.null(Logistic)){

    Results = Classification___Logistic(Logistic)

  }else if(!is.null(Bayesian)){

    Results = Classification___Bayesian(Bayesian)

  }else if(!is.null(SVM)){

    Results = Classification___SVM(SVM)

  }else if(!is.null(Decision.Trees)){

    Results = Classification___DecisionTrees(DecisionTrees)

  }

  return(Results)

}
