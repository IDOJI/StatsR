Split.Data___CV.Fold___Stratified = function(Data, Var_1, Var_2=NULL, n_fold=10, seed=1234){
  # return_test.set : test.set도 고려해서 train set을 나누나? TRUE이면 n_fold+1 개만큼 데이터가 나뉨
  # return_index : 나눠진 데이터가 아니라 그 인덱스(row)를 return하길 원하는가

  if(is.null(Var_2)){
    #===========================================================================
    # One var
    #===========================================================================
    Splitted = Split.Data___CV.Fold___Stratified___One.Var(Data, Var_1, n_fold, seed)
  }else{
    #===========================================================================
    # Two var
    #===========================================================================
    Splitted = Split.Data___CV.Fold___Stratified___Two.Var(Data, Var_1, Var_2, n_fold, seed)
  }

  return(Splitted)
}







