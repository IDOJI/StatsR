Test___Normality___Data.Frame = function(Data, Group_Var=NULL, Response_Var, alpha=0.05){
  #=============================================================================
  # Group var
  #=============================================================================
  if(!is.null(Group_Var)){
    Group = Data[,Group_Var] %>% unlist
    Response = Data[,Response_Var] %>% unlist
    Results = tapply(X = Response, INDEX = Group, function(x){
      Test___Normality___Single.Vector(x.vec = x, alpha)
    })
  #=============================================================================
  # Non-Group var
  #=============================================================================
  }else{
    Response = Data %>% select(!!Response_Var) %>% unlist() %>% unname()
    Results = Test___Normality___Single.Vector(Response, alpha)
  }


 Results %>% Test___Normality___Data.Frame___Extract.Results() %>% return()
}



