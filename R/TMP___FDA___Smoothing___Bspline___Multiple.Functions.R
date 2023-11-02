FDA___Smoothing___Bspline___Multiple.Functions = function(Signals.list,
                                                          norder = 4,
                                                          initial_lambda = exp(0),
                                                          Lfdobj = int2Lfd(2),
                                                          path_Export){
  # Data 형태
  # Signals.list[[i]] : col = 각 함수, row = domain, i = ith ROI
  #=============================================================================
  # Export path
  #=============================================================================
  path_Export = path_Export %>% path_tail_slash()
  dir.create(path_Export, F)




  #=============================================================================
  # The First Fitting
  #=============================================================================
  Smoothed_1.list = FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element(Signals.list, norder, lambda = initial_lambda, lambdas.for.each.element = NULL, grid.decimal = 1, Lfdobj = Lfdobj, path_Export = path_Export)
  Best_Lambads_1 = sapply(Smoothed_1.list, function(y){y[[2]]})




  #=============================================================================
  # The Second lambdas
  #=============================================================================
  Smoothed_2.list = FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element(Signals.list, norder, lambda = NULL, lambdas.for.each.element = Best_Lambads_1, grid.decimal = 0.1, Lfdobj = Lfdobj, path_Export = path_Export)
  Best_Lambads_2 = sapply(Smoothed_2.list, function(y){y[[2]]})




  #=============================================================================
  # The Third lambdas
  #=============================================================================
  Smoothed_3.list = FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element(Signals.list, norder, lambda = NULL, lambdas.for.each.element = Best_Lambads_2, grid.decimal = 0.01, Lfdobj = Lfdobj, path_Export = path_Export)
  Best_Lambads_3 = sapply(Smoothed_3.list, function(y){y[[2]]})





  #=============================================================================
  # The Fourth lambdas
  #=============================================================================
  Smoothed_4.list = FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element(Signals.list, norder, lambda = NULL, lambdas.for.each.element = Best_Lambads_3, grid.decimal = 0.001, Lfdobj = Lfdobj, path_Export = path_Export)




  #=============================================================================
  # Exporting png
  #=============================================================================
  Results = sapply(seq_along(Smoothed_4.list), function(n, ...){
    path_Export_New = paste0(path_Export, "ROI_", fit_length(n, 3), "___",names(Signals.list)[n])
    dir.create(path_Export_New, showWarnings = F)
    png(filename = paste0(path_Export_New, "/ROI_", fit_length(n, 3), "___log_lambda=", log(Smoothed_4.list[[n]][[2]]), ".png"))
    plot(Smoothed_4.list[[n]][[1]])
    dev.off()

    png(filename = paste0(path_Export_New, "/ROI_", fit_length(n, 3), "___Raw", ".png"))
    matplot(Signals.list[[n]], type="l")
    dev.off()
  })





  #=============================================================================
  # Exporting Smoothed Data
  #=============================================================================
  saveRDS(object = Smoothed_4.list, file = paste0(path_Export, "FDA___Smoothing___B.Spline.rds"))


  return(Smoothed_4.list)
}
