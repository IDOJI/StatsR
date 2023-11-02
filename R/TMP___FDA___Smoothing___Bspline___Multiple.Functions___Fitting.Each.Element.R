FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element = function(Signals.list,
                                                                                 norder,
                                                                                 lambda = NULL,
                                                                                 lambdas.for.each.element = NULL,
                                                                                 grid.decimal = 1,
                                                                                 Lfdobj = int2Lfd(2),
                                                                                 path_Export){
  # lambdas.for.each.element : 각 원소 행렬에 적용되는 이전의 best lambda
  #=============================================================================
  # Lambda Sequence
  #=============================================================================
  if(is.null(lambdas.for.each.element) && !is.null(lambda)){
    lambdas.for.each.element =  rep(list(lambda), times = length(Signals.list))
  }else{
    lambdas.for.each.element = as.list(lambdas.for.each.element)
  }



  #=============================================================================
  # Smoothing
  #=============================================================================
  Results.list = lapply(seq_along(Signals.list), FUN=function(k, ...){
    # gcv와 튜닝 파라미터를 이용해서 추정
    tictoc::tic()
    kth_ROI_name = names(Signals.list)[k]
    kth_ROI = Signals.list[[k]]
    kth_log_lambda = log(lambdas.for.each.element[[k]])
    kth_lambdas = exp(seq(kth_log_lambda-10*grid.decimal, kth_log_lambda+10*grid.decimal, by = grid.decimal))

    ith_Results = FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element___Single.Data.Frame(data.df = kth_ROI,
                                                                                                            rangeval = c(1, nrow(kth_ROI)),
                                                                                                            norder = norder,
                                                                                                            breaks = seq(1, nrow(kth_ROI), by = 3),
                                                                                                            lambdas = kth_lambdas,
                                                                                                            argvals = 1:nrow(kth_ROI),
                                                                                                            Lfdobj = Lfdobj,
                                                                                                            path_Export = path_Export,
                                                                                                            filename_prefix = paste0("ROI_", fit_length(k, 3), "___", kth_ROI_name)) %>% suppressWarnings()


    cat("\n", crayon::green("Smoothing :"), crayon::red(paste0(k, "th_Element")), "\n")
    tictoc::toc()
    return(ith_Results)
  })

  return(Results.list)
}
