FDA___Smoothing___Bspline___ = function(Bslpline, best.criterion = "gcv", path_Export){
  #=============================================================================
  # Input
  #=============================================================================
  # Bspline = list(y = y,
  #                x = x,
  #                range_vals = NULL,
  #                nbasis = NULL,
  #                norder = NULL,
  #                breaks = NULL,
  #                labmdas = NULL,
  #                m_int2Lfd = NULL)
  #-------------------------------
  # y & x
  #-------------------------------
  y = Bspline$y
  x = Bspline$x



  #-------------------------------
  # range_vals
  #-------------------------------
  range_vals = Bspline$range_vals
  if(is.null(range_vals)){
    range_vals = c(min(x), max(x))
  }
  # rangeval = c(1,length(y))



  #-------------------------------
  # nbasis
  #-------------------------------
  nbasis = Bspline$nbasis



  #-------------------------------
  # norder
  #-------------------------------
  norder = Bspline$norder
  if(is.null(norder)){
    norder = 4
  }




  #-------------------------------
  # breaks
  #-------------------------------
  # break 오류가 나면 중복값 있는지 확인
  breaks = Bspline$breaks
  if(is.null(breaks)){
    if(is.null(x)){
      # breaks =
      # knots = seq(1, length(y), length.out = jth_length.out)
    }else{
      breaks = x
    }
  }



  #-------------------------------
  # lambdas
  #-------------------------------
  lambdas = Bspline$labmdas
  if(is.null(lambdas)){
    lambdas = exp(-100:100)
  }




  #-------------------------------
  # m_int2Lfd
  #-------------------------------
  m_int2Lfd = Bspline$m_int2Lfd
  if(is.null(m_int2Lfd)){
    m_int2Lfd = 2 # Curvature : the rate of change of slope
  }





  #=============================================================================
  # 1) Basis Object
  #=============================================================================
  basis_obj = fda::create.bspline.basis(rangeval = range_vals,
                                        # nbasis = nbasis,
                                        norder = norder,
                                        breaks = breaks %>% unname %>% as.numeric)

  # eval_basis = eval.basis(evalarg = seq(range_vals[1], range_vals[2], by = 0.01), basisobj = basis_obj)
  # matplot(x = seq(range_vals[1], range_vals[2], by = 0.01), y = eval_basis, type = "l")








  #=============================================================================
  # 2) Functional Data Object & Smoothing
  #=============================================================================
  if(null(m_int2Lfd)){

    fd_par_obj = fdPar(fdobj = basis_obj)

  }else{

    gcv = sapply(lambdas, function(ith_lambda){


      tryCatch({

        fd_par_obj = fdPar(fdobj = basis_obj,
                           Lfdobj = int2Lfd(m_int2Lfd),
                           lambda = ith_lambda)


        smoothing = smooth.basis(argvals = breaks, y = y, fdParobj = fd_par_obj)

        mean(smoothing$gcv) # mean gcv

      }, error = function(e) {
        return(NA)
      })
    })


    ind = which.min(gcv)


    fd_par_obj = fdPar(fdobj = basis_obj,
                       Lfdobj = int2Lfd(m_int2Lfd),
                       lambda = lambdas[ind])


    smoothing = smooth.basis(argvals = breaks, y = y, fdParobj = fd_par_obj)


  }
}






