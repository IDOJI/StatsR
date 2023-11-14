Classification___Logistic___Multinomial___Nominal___MLE = function(){



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
#
# # Assuming you have a matrix 'X' containing your predictor variables and a vector 'y' containing your binary response variable.
# # Replace 'X' and 'y' with your actual data.
#
# # Load the glmnet package if not already loaded
# if (!require(glmnet)) {
#   install.packages("glmnet")
#   library(glmnet)
# }
#
# Splitted_New$y_Train = factor(Splitted_New$y_Train, levels = c("CN", "AD"))
# diagnosis_binary <- ifelse(Splitted_New$y_Train == "AD", 1, 0)
# # Fit the logistic regression model using glmnet
# model <- glmnet(x = Splitted_New$X_Train, y = Splitted_New$y_Train, family = "binomial")
#
# # Optionally, you can perform cross-validation to select the optimal lambda value.
# # Here's an example using cross-validation:
# cv_model <- cv.glmnet(x = as.matrix(Splitted_New$X_Train), y = diagnosis_binary, family = "binomial", foldid = Splitted_New$X_Train_CV_Ind)
#
#
#
#
#
#
#
#
#
#
# library(glmnet)
#
# # Assuming you have your predictors in X_train and binary response in y_train
#
# # Create a matrix of predictors
# X_matrix <- as.matrix(Splitted_New$X_Train)
#
#
# # Convert the binary response to a numerical vector (0 and 1)
# y_vector <- factor(diagnosis_binary, levels=c(0,1))
#
# # Specify the values of lambda (regularization strength) you want to consider
# lambda_seq <- 10^seq(0, -5, by = -0.1)  # Adjust the range as needed
#
# # Fit the logistic regression model with L1 regularization using cv.glmnet
# cv_model <- cv.glmnet(
#   x = X_matrix,
#   y = y_vector,
#   family = "binomial",  # For binary logistic regression
#   alpha = 1,            # Set alpha to 1 for L1 regularization (Lasso)
#   lambda = lambda_seq,  # Sequence of lambda values
#   foldid = Splitted_New$X_Train_CV_Ind           # Number of cross-validation folds
# )
#
# # Find the optimal lambda
# best_lambda <- cv_model$lambda.min
# cat("Optimal Lambda:", best_lambda, "\n")
#
# # Fit the final model with the optimal lambda
# final_model <- glmnet(
#   x = X_matrix,
#   y = y_vector,
#   family = "binomial",
#   alpha = 0.5,            # L1 regularization
#   lambda = best_lambda
# )
#
# # Print the model coefficients
# coef(final_model)
#
#
#
#
# library(MASS)
# library(glmnet)
# set.seed(8675309)
# ### simulate covariates from inepedent normals
# xx <- mvrnorm(100, rep(0, 100), diag(1, 100))
# ### simulate coefficients first 50 from beta second 50 0 => spurious features
# my_beta <- c(runif(50, -2, 2), rep(0, 50))
# ## simulate responses
# yy <- rbinom(100, 1, plogis(xx %*% my_beta))
#
# ## do your glmnet
# reg <- cv.glmnet(x = xx, y = yy, alpha  = 0.5, family = 'binomial')
#
#
# rownames(coef(reg, s = 'lambda.min'))[coef(reg, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#
# rownames(coef(cv_model, s = 'lambda.min'))[coef(cv_model, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#
#
#
#
#
#
# dim(Splitted_New$X_Train)
# Splitted_New$y_Train %>% table
#
#
#
#
# # Print the lambda value with the minimum cross-validated error
# best_lambda <- cv_model$lambda.min
# cat("Best Lambda:", best_lambda, "\n")
#
# # To make predictions on new data (replace 'X_new' with your new data matrix):
# predictions <- predict(model, newx = X_new, s = best_lambda, type = "response")
#
# # You can then set a threshold to classify the predictions as 0 or 1 based on your problem's requirements
# # For example, if the threshold is 0.5, predictions >= 0.5 are classified as 1, and predictions < 0.5 are classified as 0.
# predicted_classes <- ifelse(predictions >= 0.5, 1, 0)
#
# # Evaluate the model's performance using metrics like accuracy, ROC curve, etc.
#
#
#
#
#
#
#
# #===============================================================================
# # Classification
# #===============================================================================
# library(glmnet)
#
# # Fit a multinomial logistic regression model using glmnet with your CV index
# # Make sure you have the 'cv.glmnet' package installed for cross-validation
# tictoc::tic()
# fit = cv.glmnet(as.matrix(Splitted_1$X_Train), y = Splitted_1$y_Train, family = "multinomial", foldid = Splitted_1$X_Train_CV_Ind)
#
#
#
# tictoc::toc()
# ?cv.glmnet
#
# # Find the optimal lambda value
# best_lambda <- fit$lambda.min
#
# # Fit the final model with the optimal lambda
# final_model = glmnet(x = as.matrix(Splitted_1$X_Train), y = Splitted_1$y_Train, family = "multinomial", lambda = best_lambda)
# final_model
#
#
# # Make predictions on the test data
# predictions <- predict(final_model, newx = as.matrix(Test_1[, -c(1:2)]), s = best_lambda, type = "class")
#
# predictions <- factor(predictions, levels = levels(Test_1$DIAGNOSIS))
#
# # Evaluate the model's performance
# confusionMatrix(predictions, Test_1$DIAGNOSIS)
#
#
# Test = Test_1[, -c(1:2)]
#
#
#
#
#
# n = 500
# p = 30
# nzc = trunc(p/10)
# x = matrix(rnorm(n * p), n, p)
# beta3 = matrix(rnorm(30), 10, 3)
# beta3 = rbind(beta3, matrix(0, p - 10, 3))
# f3 = x %*% beta3
# p3 = exp(f3)
# p3 = p3/apply(p3, 1, sum)
# g3 = glmnet:::rmult(p3)
# set.seed(10101)
# cvfit = cv.glmnet(x, g3, family = "multinomial")
# plot(cvfit)
# title("Multinomial Family", line = 2.5)
#
