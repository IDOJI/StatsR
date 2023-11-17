Classification___Logistic___Binomial = function(Logistic){

  if(Logistic$Fitting_Method == "MLE"){

    Logistic = Classification___Logistic___Binomial___MLE(Logistic)

  }else{

    Logistic = Classification___Logistic___Binomial___Penalty(Logistic)

  }

  return(Logistic)

}
