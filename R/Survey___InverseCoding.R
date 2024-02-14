Survey___InverseCoding = function(data.df, cols=NULL, max_score, min_score){
  # cols
  if(is.null(cols)){
    cols = names(data.df)
  }
  # max score length
  if(length(max_score) != length(cols)){
    max_score = rep(max_score, times = length(cols))
  }
  # min score length
  if(length(min_score) != length(cols)){
    min_score = rep(min_score, times = length(cols))
  }


  for(i in 1:length(cols)){
    ith_max_score = max_score[i]
    ith_min_score = min_score[i]
    ith_col = data.df[,cols[i]]
    data.df[,cols[i]] = ith_max_score + ith_min_score - ith_col
  }

  return(data.df)
}
