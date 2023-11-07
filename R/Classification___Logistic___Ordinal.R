Classification___Logistic___Ordinal = function(Logistic){
  # which method?
  Fitting_Method = Logistic$Fitting_Method
  Fitting_Method_Choices = c("MLE", "ElasticNet") %>% tolower()
  Fitting_Method = match.arg(tolower(Fitting_Method), Fitting_Method_Choices)


  # MLE
  if(Fitting_Method == "mle"){

    Results = Classification___Logistic___Ordinal___MLE(Logistic)

  # Elastic Net
  }else if(Fitting_Method == "elasticnet"){

    Results = Classification___Logistic___Ordinal___Elastic(Logistic)

  }



  return(Results)
}



















