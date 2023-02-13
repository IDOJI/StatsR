SUB___Have.Outlier = function(x, method="IQR"){
  if(method=="IQR"){
    upper_Q = fivenum(x)[4]
    lower_Q = fivenum(x)[2]
    IQR = upper_Q - lower_Q


    ind_upper = which(x > upper_Q + 1.5*IQR)
    ind_lower = which(x < lower_Q - 1.5*IQR)
    upper_outlier = x[ind_upper]
    lower_outlier = x[ind_lower]

    if(length(upper_outlier) > 0 && length(lower_outlier) > 0){
      return(c(ind_upper, ind_lower))
    }else{
      return(FALSE)
    }
  }
}
