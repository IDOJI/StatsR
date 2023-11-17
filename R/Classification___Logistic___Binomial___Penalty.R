Classification___Logistic___Binomial___Penalty = function(Logistic){
  #=============================================================================
  # Group Penalty
  #=============================================================================
  if(is.null(Logistic$Grouped_Vars_Index)){

    Logistic = Classification___Logistic___Binomial___Penalty___NonGrouped(Logistic)

  }else{

    Logistic = Classification___Logistic___Binomial___Penalty___Grouped(Logistic)

  }


  return(Logistic)
}





