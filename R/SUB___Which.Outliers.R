sub___which.outliers = function(x, method="IQR", as.index=TRUE){
  if(method == "IQR"){
    # 사분위수 계산
    Q1 = quantile(x, 0.25)
    Q3 = quantile(x, 0.75)
    IQR = Q3 - Q1

    # 이상치 인덱스 계산
    ind_upper = which(x > Q3 + 1.5 * IQR)
    ind_lower = which(x < Q1 - 1.5 * IQR)
    outliers_index = c(ind_upper, ind_lower)

    # 결과 반환
    if(length(outliers_index) > 0){
      if(as.index){
        return(outliers_index)
      }else{
        return(x[outliers_index])
      }
    } else {
      return(integer(0))  # 빈 정수 벡터 반환
    }
  } else {
    stop("Unsupported method")
  }
}
