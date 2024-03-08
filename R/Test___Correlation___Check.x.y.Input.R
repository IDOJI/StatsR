Test___Correlation___Check.x.y.Input = function(df, y, x=NULL){
  # if x is NULL
  if(is.null(x)){
    x = colnames(df)[colnames(df) != y]
  }


  # both character & colnames
  if(is.character(x) && is.character(y)){
      X = df[,x] %>% as_tibble() %>% setNames(x)
      Y = df[,y] %>% as_tibble() %>% setNames(y)
  }else{
      stop("x and y should be Character")
  }


  return(list(X = X, Y = Y))
}
