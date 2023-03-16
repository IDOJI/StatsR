FDA___Smoothing___Bspline = function(y, knots_length.out = 2:floor((length(y)/4)), lambdas, penalty.type = "second", file.name_prefix = "", save.path = NULL){
  ##############################################################################
  if(penalty.type == "harmonic" || penalty.type == "Harmonic"){
    # results.list = lapply(lambdas, FUN=function(ith_lambda, ...){
    #   FDA___Smoothing___Bspline___Pen.Harmonic(y = y, nbasis = n_basis, lambda = ith_lambda) %>% suppressWarnings()
    # })
  }else if(penalty.type == "Second" || penalty.type == "second"){
    results.list = lapply(lambdas, FUN=function(ith_lambda, ...){
      tryCatch(FDA___Smoothing___Bspline___Pen.Second(y, ith_lambda, length.out_seq = knots_length.out, norders = 4:20),
               error = function(e){NULL},
               warning = function(w){NULL},
               finally = NULL)
    }) %>% rm_list_null
  }else if(is.null(penalty.type)){

  }
  ### The minimum GCV & Plotting out
  GCVs = sapply(results.list, FUN=function(ith_results){
    ith_results[[1]]$gcv
  })
  selected_results.list = results.list[[which.min(GCVs)]]
  FDA___Smoothing___Plotting(y,
                             knots = seq(1, length(y), length.out = selected_results.list$knots_length),
                             smoothed = selected_results.list$smoothed,
                             main = paste0(file.name_prefix, "___norder-", selected_results.list$norder, "_", "knots_length-", selected_results.list$knots_length, "_", "lambda-", selected_results.list$lambda, "_", "LogLambda-", log(selected_results.list$lambda)),
                             file.name = paste0(file.name_prefix, "___", "norder-", selected_results.list$norder, "_", "knots_length-", selected_results.list$knots_length, "_", "lambda-",selected_results.list$lambda, "_", "LogLambda-", log(selected_results.list$lambda), ".png"),
                             save.path = save.path)
  ###
  return(selected_results.list)
}
