FDA___Smoothing___Bspline___Total = function(y, knots_length.out = 2:floor((length(y)/4)), lambdas, penalty.type = NULL, file.name_prefix=NULL, save.path=NULL){
  ### seq_1 ============================================================================================
  opt_smoothing_1 = FDA___Smoothing___Bspline(y, knots_length.out, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The first smoothing is done. !"), "\n")
  # FDA___Smoothing___Plotting(y = y, knots = seq(1, length(y), length.out = opt_smoothing_1$knots_length), smoothed = opt_smoothing_1$smoothed)
  # log(opt_smoothing_1$lambda)
  ### seq_2 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_1$lambda) - 5, log(opt_smoothing_1$lambda) + 5, by = 0.1))
  opt_smoothing_2 = FDA___Smoothing___Bspline(y, knots_length.out, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The second smoothing is done. !"), "\n")
  # FDA___Smoothing___Plotting(y = y, knots = seq(1, length(y), length.out = opt_smoothing_2$knots_length), smoothed = opt_smoothing_2$smoothed)
  # log(opt_smoothing_2$lambda)
  ### seq_3 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_2$lambda) - 0.5, log(opt_smoothing_2$lambda) + 0.5, by = 0.01))
  opt_smoothing_3 = FDA___Smoothing___Bspline(y, knots_length.out, lambdas, penalty.type, save.path = save.path)
  cat("\n", crayon::yellow("The third smoothing is done. !"), "\n")
  # FDA___Smoothing___Plotting(y = y, knots = seq(1, length(y), length.out = opt_smoothing_3$knots_length), smoothed = opt_smoothing_3$smoothed)
  # log(opt_smoothing_3$lambda)
  ### seq_4 ============================================================================================
  lambdas = exp(seq(log(opt_smoothing_3$lambda) - 0.05 , log(opt_smoothing_3$lambda) + 0.05 , by = 0.001))
  opt_smoothing_4 = FDA___Smoothing___Bspline(y, knots_length.out, lambdas, penalty.type, save.path)
  FDA___Smoothing___Plotting(y = y, knots = seq(1, length(y), length.out = opt_smoothing_4$knots_length), smoothed = opt_smoothing_4$smoothed)
  cat("\n", crayon::yellow("The final smoothing is done. !"), "\n")
  return(opt_smoothing_4)
}
