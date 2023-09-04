Classification___Multinomial___Oridinal = function(X, y,
                                                   method = c("MLE", "Elastic"),
                                                   penatly_alpha = NULL,
                                                   penalty_lambda = NULL) {

  #=============================================================================
  # method
  #=============================================================================
  # Convert choices to lowercase for case-insensitive comparison
  method_choices = tolower(method)

  method = match.arg(tolower(method))

  if(!method %in% method_choices) {
    stop("Invalid method choice.")
  }





  #=============================================================================
  # Analysis by method
  #=============================================================================
  # Code for MLE method
  if(method == "mle"){
   Results = Classification___Multinomial___Oridinal___MLE()



  # Code for Elastic method
  }else if(method == "elastic"){
    Results = Classification___Multinomial___Oridinal___Elastic.Net()
  }





  #=============================================================================
  # Return
  #=============================================================================
  return(Results)
}









Classification___Multinomial___Oridinal___MLE = function(){
  #=============================================================================
  # Install.pacakges
  #=============================================================================
  install_packages("MASS")



  #=============================================================================
  # Install.pacakges
  #=============================================================================
  # Generate example data (ordinal outcome)
  data$y_ord <- factor(ifelse(data$x1 + data$x2 + rnorm(100) > 1, "High",
                              ifelse(data$x1 + data$x2 + rnorm(100) > 0, "Medium", "Low")))

  # Fit proportional odds model
  model_polr <- polr(y_ord ~ x1 + x2, data = data, Hess=TRUE)
  summary(model_polr)
}



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




