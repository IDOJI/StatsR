Test___Normality = function(Data, Group_Var=NULL, Response_Vars=NULL, alpha = 0.05){
  #=============================================================================
  # Single vector
  #=============================================================================
  if(is.null(Group_Var) && is.null(Response_Vars)){
    Results = Test___Normality___Single.Vector(Data, alpha)






  #=============================================================================
  # Data.frame with group var
  #=============================================================================
  }else if(!is.null(Response_Vars)){
    Results = lapply(Response_Vars, function(ith_Response, ...){
      Test___Normality___Data.Frame(Data = Data, Group_Var = Group_Var, Response_Var = ith_Response, alpha = alpha)
    })
    names(Results) = Response_Vars







  #=============================================================================
  # Else
  #=============================================================================
  }else{
    stop("Check input!")
  }



  cat("\n", crayon::green("Testing"), crayon::red("Normality"), crayon::green("is done!"),"\n")
  return(Results)

}



