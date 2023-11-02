FDA___Smoothing___Bspline___Pen.Second = function(y, lambda, length.out_seq, norders=4:20, save.path=NULL){
  ##############################################################################
  ### n_order
  smoothed_norders.list = lapply(norders, FUN=function(ith_norder, ...){
    ### knots
    smoothed_knots.list = lapply(length.out_seq, FUN=function(jth_length.out, ...){



      smoothed = tryCatch(smooth.basis(argvals = 1:length(y),
                                       y = y,
                                       fdParobj = penalty_Par),
                          error = function(e){NULL},
                          warning = function(w){NULL},
                          finally = NULL)
      # main = paste0("order_", ith_order, "_", "length.out_", jth_length.out)
      # FDA___Smoothing___Plotting(y, knots, smoothed, main, file.name = paste0(main, ".png"), save.path)
      return(smoothed)
    }) %>% rm_list_null
    ### selected minimum
    which.min_gcv = FDA___Smoothing___Min.Criteria.For.List(smoothed_knots.list)
    min_gcv_smoothed_knots.list = smoothed_knots.list[[which.min_gcv]]
    min_gcv_norder = ith_norder
    min_gcv_lambda = lambda
    min_gcv_knots_length = length.out_seq[which.min_gcv]
    min_gcv_smoothed = list(smoothed = min_gcv_smoothed_knots.list,
                            norder = min_gcv_norder,
                            lambda = min_gcv_lambda,
                            knots_length = min_gcv_knots_length)

    ### plotting
    # FDA___Smoothing___Plotting(y = y,
    #                            knots = seq(1, length(y), length.out = length.out_seq[which.min_gcv]),
    #                            smoothed = smoothed_knots.list[[which.min_gcv]],
    #                            main = paste0("norder", "-", ith_norder, "_", "\n", "knots.length", "-", length.out_seq[which.min_gcv], "_", "\n", "_LogLambda-", log(lambda)),
    #                            file.name = paste0("norder", "-", min_gcv_norder, "_", "knots.length", "-", length.out_seq[which.min_gcv], "_", "lambda", "-", lambda, "_LogLambda-", log(lambda), ".png"),
    #                            save.path = NULL)
    return(min_gcv_smoothed)
  }) %>% rm_list_null
  ##############################################################################
  GCVs = sapply(smoothed_norders.list, FUN=function(ith_smoothed.list){
    ith_smoothed.list$smoothed$gcv
  })
  ##############################################################################
  # OCVs = sapply(smoothed_norders.list, FUN=function(ith_smoothed.list){
  #   P = ith_smoothed.list$smoothed$y2cMap
  #   P %*% solve( t(P) %*% P ) %*% t(P)
  #   ith_smoothed.list = smoothed_norders.list[[1]]
  #   ith_smoothed.list$smoothed$gcv
  # })
  ##############################################################################
  smoothed_norders.list[[which.min(GCVs)]]
}
