Classification___Multinomial___Nominal___MLE = function(){



  install.packages("nnet")
  library(nnet)

  # Generate example data
  set.seed(123)
  data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  data$y <- factor(ifelse(data$x1 + data$x2 + rnorm(100) > 0, "B",
                          ifelse(data$x1 - data$x2 + rnorm(100) < 0, "A", "C")))

  # Fit multinomial logit model
  model_multinom <- multinom(y ~ x1 + x2, data = data)
  summary(model_multinom)

}
