FDA___Smoothing___Fourier___Total = function(y = y, n_basis = nbasis, lambdas = lambdas, penalty.type = "harmonic", criterion = c("OCV", "GCV")){
  ### seq_1
  opt_smoothing_1 = FDA___Smoothing___Fourier(y = y, n_basis = nbasis, lambdas = lambdas, penalty.type = penalty.type, criterion = criterion)
  # plot(y)
  # lines(x = 1:length(y), y = opt_smoothing_1$y.hat, col="red")
  # log(opt_smoothing_1$lambda)
  ### seq_2
  lambdas = exp(seq(log(opt_smoothing_1$lambda) - 5, log(opt_smoothing_1$lambda) + 5, by = 0.1))
  opt_smoothing_2 = FDA___Smoothing___Fourier(y = y, n_basis = nbasis, lambdas = lambdas, penalty.type = penalty.type, criterion = criterion)
  # plot(y)
  # lines(x = 1:length(y), y = opt_smoothing_2$y.hat, col="red")
  # log(opt_smoothing_2$lambda)
  ### seq_3
  lambdas = exp(seq(log(opt_smoothing_2$lambda) - 0.5, log(opt_smoothing_2$lambda) + 0.5, by = 0.01))
  opt_smoothing_3 = FDA___Smoothing___Fourier(y = y, n_basis = nbasis, lambdas = lambdas, penalty.type = penalty.type, criterion = criterion)
  # plot(y)
  # lines(x = 1:length(y), y = opt_smoothing_3$y.hat, col="red")
  # log(opt_smoothing_3$lambda)
  ### seq_4
  lambdas = exp(seq(log(opt_smoothing_3$lambda) - 0.05 , log(opt_smoothing_3$lambda) + 0.05 , by = 0.001))
  opt_smoothing_4 = FDA___Smoothing___Fourier(y = y, n_basis = nbasis, lambdas = lambdas, penalty.type = penalty.type, criterion = criterion)
  # plot(y)
  # lines(x = 1:length(y), y = opt_smoothing_4$y.hat, col="red")
  # log(opt_smoothing_4$lambda)
  return(opt_smoothing_4)
}
