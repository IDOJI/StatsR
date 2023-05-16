SUB___Which.Outliers = function(x, method="IQR", as.index=T){
  if(method=="IQR"){
    upper_Q = fivenum(x)[4]
    lower_Q = fivenum(x)[2]
    IQR = upper_Q - lower_Q


    ind_upper = which(x > upper_Q + 1.5*IQR)
    ind_lower = which(x < lower_Q - 1.5*IQR)
    outliers_index = c(ind_upper, ind_lower)


    if(length(outliers_index) > 0){
      if(as.index){
        return(outliers_index)
      }else{
        return(x[outliers_index])
      }
    }else{
      return(FALSE)
    }
  }
}
