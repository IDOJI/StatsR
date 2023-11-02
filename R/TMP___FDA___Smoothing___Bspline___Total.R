FDA___Smoothing___Bspline___Total = function(y, knots_length.out = 2:floor((length(y)/4)), norders, lambdas, penalty.type = NULL, file.name_prefix=NULL, save.path=NULL){
  ### seq_1 ============================================================================================
  opt_smoothing_1 = FDA___Smoothing___Bspline(y, knots_length.out, norders, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The first smoothing is done. !"), "\n")
  FDA___Smoothing___Plotting(y,
                             knots = seq(1, length(y), length.out = opt_smoothing_1$knots_length),
                             smoothed = opt_smoothing_1$smoothed,
                             main = paste0(file.name_prefix, "\n", "___norder-", opt_smoothing_1$norder, "_", "knots_length-", opt_smoothing_1$knots_length, "_", "LogLambda-", log(opt_smoothing_1$lambda)),
                             save.path = save.path)
  # log(opt_smoothing_1$lambda)
  ### seq_2 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_1$lambda) - 1, log(opt_smoothing_1$lambda) + 1, by = 0.1))
  opt_smoothing_2 = FDA___Smoothing___Bspline(y, knots_length.out, norders, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The second smoothing is done. !"), "\n")
  FDA___Smoothing___Plotting(y,
                             knots = seq(1, length(y), length.out = opt_smoothing_2$knots_length),
                             smoothed = opt_smoothing_2$smoothed,
                             main = paste0(file.name_prefix, "\n", "___norder-", opt_smoothing_2$norder, "_", "knots_length-", opt_smoothing_2$knots_length, "_", "LogLambda-", log(opt_smoothing_2$lambda)),
                             save.path = save.path)


  ### seq_3 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_2$lambda) - 0.1, log(opt_smoothing_2$lambda) + 0.1, by = 0.01))
  opt_smoothing_3 = FDA___Smoothing___Bspline(y, knots_length.out, norders, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The third smoothing is done. !"), "\n")
  FDA___Smoothing___Plotting(y,
                             knots = seq(1, length(y), length.out = opt_smoothing_3$knots_length),
                             smoothed = opt_smoothing_3$smoothed,
                             main = paste0(file.name_prefix, "\n", "___norder-", opt_smoothing_3$norder, "_", "knots_length-", opt_smoothing_3$knots_length, "_", "LogLambda-", log(opt_smoothing_3$lambda)),
                             save.path = save.path)



  ### seq_4 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_3$lambda) - 0.01 , log(opt_smoothing_3$lambda) + 0.01 , by = 0.001))
  opt_smoothing_4 = FDA___Smoothing___Bspline(y, knots_length.out, norders, lambdas, penalty.type, save.path)
  FDA___Smoothing___Plotting(y,
                             knots = seq(1, length(y), length.out = opt_smoothing_4$knots_length),
                             smoothed = opt_smoothing_4$smoothed,
                             main = paste0(file.name_prefix, "\n", "___norder-", opt_smoothing_4$norder, "_", "knots_length-", opt_smoothing_4$knots_length, "_", "LogLambda-", log(opt_smoothing_4$lambda)),
                             file.name = paste0(paste0(file.name_prefix, "___norder-", opt_smoothing_4$norder, "_", "knots_length-", opt_smoothing_4$knots_length, "_", "LogLambda-", log(opt_smoothing_4$lambda)), ".png"),
                             save.path = save.path)
  cat("\n", crayon::yellow("The final smoothing is done. !"), "\n")
  return(opt_smoothing_4)
}
