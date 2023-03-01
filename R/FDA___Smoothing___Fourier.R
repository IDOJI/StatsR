FDA___Smoothing___Fourier = function(y, n_basis=NULL, lambdas, penalty.type = c("second", "harmonic"), criterion=c("ocv", "gcv")){
  ##############################################################################
  if(penalty.type == "harmonic" || penalty.type == "Harmonic"){
    results.list = lapply(lambdas, FUN=function(ith_lambda){
      FDA___Smoothing___Fourier___Pen.Harmonic(y = y, nbasis = n_basis, lambda = ith_lambda)
    })
  }else if(penalty.type == "Second" || penalty.type == "second"){
    results.list = lapply(lambdas, FUN=function(ith_lambda){
      FDA___Smoothing___Fourier___Pen.Second(y = y, nbasis = n_basis, lambda = ith_lambda)
    })
  }
  ##############################################################################
  GCV = sapply(results.list, FUN=function(ith){
    ith$GCV
  })
  ##############################################################################
  OCV = sapply(results.list, FUN=function(ith){
    ith$OCV
  })
  ##############################################################################

  ##############################################################################
  if(which.min(GCV)==which.min(OCV)){
    results.list[[which.min(OCV)]] %>% return
  }else{
    if(criterion == "gcv" || criterion == "GCV"){
      results.list[[which.min(GCV)]] %>% return
    }else if(criterion == "ocv" || criterion == "OCV"){
      results.list[[which.min(OCV)]] %>% return
    }
  }
}
