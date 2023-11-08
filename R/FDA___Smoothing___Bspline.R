FDA___Smoothing___Bspline = function(Bspline, best.criterion = "gcv", path_Export=NULL, file.name=NULL){
  #=============================================================================
  # 0.Input
  #=============================================================================
  # Bspline = list(y = y,
  #                x = x,
  #                range_vals = NULL,
  #                nbasis = NULL,
  #                norder = NULL,
  #                breaks = NULL,
  #                labmdas = NULL,
  #                m_int2Lfd = NULL,
  #                argvals = NULL)
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
  # breaks = c(Bspline$breaks[1], seq(7,10, 0.1), Bspline$breaks[-1])
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





  #-------------------------------
  # argvals
  #-------------------------------
  argvals = Bspline$argvals
  if(is.null(argvals)){
    argvals = 1:(dim(y)[1])
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
  if(is.null(m_int2Lfd)){
    #---------------------------------
    # non penalty
    #---------------------------------
    fd_par_obj = fda::fdPar(fdobj = basis_obj)

    smoothing = fda::smooth.basis(argvals = breaks, y = y, fdParobj = fd_par_obj)

    best_lambda = NULL

  }else{
    #---------------------------------
    # mean gcv from all curves
    #---------------------------------
    gcv = sapply(lambdas, function(ith_lambda){

      tryCatch({
        fd_par_obj = fda:::fdPar(fdobj = basis_obj,
                                 Lfdobj = int2Lfd(m_int2Lfd),
                                 lambda = ith_lambda)

        # smoothing = fda::smooth.basis(argvals = breaks, y = y, fdParobj = fd_par_obj)
        smoothing = fda::smooth.basis(argvals = argvals, y = y, fdParobj = fd_par_obj)


        return(mean(smoothing$gcv)) # mean gcv

      }, error = function(e) {
        return(NA)
      })
    })
    if(is.na(gcv) %>% sum == length(gcv)){
      stop("All lambdas are error!")
    }





    #---------------------------------
    # Best Smoothing
    #---------------------------------
    best_ind = which.min(gcv)

    best_lambda = lambdas[best_ind]


    fd_par_obj = fda::fdPar(fdobj = basis_obj,
                            Lfdobj = int2Lfd(m_int2Lfd),
                            lambda = best_lambda)

    smoothing = fda::smooth.basis(argvals = argvals,
                                  y = y,
                                  fdParobj = fd_par_obj)
    # plot(smoothing)
  }

  Results = list(smoothing = smoothing, best_lambda = best_lambda)







  #=============================================================================
  # 3) Plotting & Exporting data
  #=============================================================================
  if(!is.null(path_Export)){
    #---------------------------------------
    # dir
    #---------------------------------------
    fs::dir_create(path_Export, recurse = T)






    #---------------------------------------
    # plot
    #---------------------------------------
    png(filename = paste0(path_Export, "/", file.name, ".png"), bg = "white", width = 5000, height = 1000)
    par(mfrow=c(1,3))

    # gcv
    plot(gcv, main = "gcv with best index")
    abline(v=best_ind, col = 'red')

    # matplot
    matplot(x=x, y=y, type= "l", main = paste0("Raw Signals : ", file.name))

    # smoothing
    plot(smoothing, main = paste0("Smoothing : ", "lambda=", round(best_lambda, 4)))
    dev.off()






    #---------------------------------------
    # Data saving
    #---------------------------------------
    saveRDS(Results, file = paste0(path_Export, "/", file.name, ".rds"))
    cat("\n", crayon::red(file.name), crayon::green("is done!"), "\n")
  }


  return(Results)
}











