Classification___Logistic___Binomial = function(Logistic){

  if(Logistic$Fitting_Method == "MLE"){

    Results = Classification___Logistic___Binomial___MLE(Logistic)

  }else if(Logistic$Fitting_Method == "Elastic"){

    Results = Classification___Logistic___Binomial___Elastic(Logistic)

  }

  return(Results)

}
