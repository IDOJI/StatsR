Classification___Logistic___Ordinal___Elastic = function(Logistic){

  if(is.null(Logistic$Grouped_Vars_Index)){

    Results = Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty(Logistic)

  }else{

    Results = Classification___Logistic___Ordinal___Elastic___GroupedPenalty(Logistic)

  }

  return(Results)
}





