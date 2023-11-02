FDA___Smoothing___Min.Criteria.For.List = function(Smoothed.list){
  GCVs = sapply(Smoothed.list, FUN=function(ith_smoothed, ...){
    ith_smoothed$gcv
    # P = ith_smoothed$y2cMap
    # Hat.mat = t(P) %*% solve( t(P) %*% P ) %*% P
    # OCV = mean( (y-eval.fd(1:length(y), ith_smoothed$fd))^2 / (1-diag(Hat.mat))^2 )
  })
  which.min(GCVs)
}
