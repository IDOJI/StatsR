Classification___Logistic___Binomial___Elastic = function(Logistic){
#   # Load the glmnet library
#   install_packages("glmnet")
#
#   Combined_X = rbind(Logistic$Train_X, Logistic$Test_X)[,-5]
#   Combined_y = rbind(Logistic$Train_y, Logistic$Test_y)
#
#
#
#   Combined_X_new = Combined_X[,1:5]
#
#
#   test = glmnet(x = Combined_X_new, y = Combined_y %>% unlist, family = "binomial")
#
# Logistic$Best_Model  = test
#
#
#   library(glmnet)
#   library(pROC)
#
#   # Assuming 'test' is your fitted glmnet model object,
#   # and you have a test dataset 'test_X' and true outcomes 'test_y'
#
#   # Generate predicted probabilities for the test set
#   predicted_probabilities <- predict(test, newx = Combined_X_new, type = "response")
#
#   dim(predicted_probabilities)
#
#   # The predicted_probabilities are a matrix. For binomial, glmnet returns a two-column matrix
#   # with the first column being the "0" class and the second being the "1" class
#   # We will use the second column (the "1" class probabilities)
#   predicted_probabilities <- predicted_probabilities[, 2]
#
#   # Compute the AUC using pROC
#   roc_obj <- roc(Combined_y %>% unlist, predicted_probabilities)
#   auc_value <- auc(roc_obj)
#   print(auc_value)
#
#   # Plot the ROC curve
#   plot(roc_obj, main = sprintf("ROC Curve (AUC = %.2f)", auc_value))
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
#
#
#   colnames(Combined_X) %>% head(5)
#
#
#   Logistic$penalty_factor = c(rep(0, 5) , rep(1, n_Vars-6))
#
#   # Test
#   Logistic$Best_Model = Fit = glmnet(x = Combined_X,
#                                      y = Combined_y %>% unlist,
#                                      family = Logistic$Family,
#                                      alpha = 1,
#                                      lambda = Logistic$penalty_lambda[30],
#                                      penalty.factor = Logistic$penalty_factor)
#
#
#
#
#   write.csv(Logistic$Best_Coef, file = paste0(Logistic$path_Export, "/coef.csv"))
#
#
#   library(glmnet)
#   library(pROC)
#
#   # Assuming 'Logistic$Best_Model' is your fitted glmnet model object,
#   # and you have a validation dataset 'validation_X' and true outcomes 'validation_y'
#
#   # Generate predicted probabilities for the validation set
#   predicted_probabilities <- predict(Logistic$Best_Model, newx = Combined_X, type = "response", s = Logistic$penalty_lambda[40])
#
#   # Compute the AUC using pROC
#   roc_obj <- roc(Combined_y %>% unlist, predicted_probabilities)
#   auc_value <- auc(roc_obj)
#   print(auc_value)
#
#
#
#   # If you want to plot the ROC curve
#   plot(roc_obj, main = sprintf("ROC Curve (AUC = %.2f)", auc_value))
#
#
#
#
#
#   Logistic$Fit = Fit = glmnet(x = Logistic$Train_X,
#                               y = Logistic$Train_y %>% unlist,
#                               family = Logistic$Family,
#                                alpha = 0.5,
#                                lambda = Logistic$penalty_lambda[40],
#                                penalty.factor = Logistic$penalty_factor)
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
#
#   # Fit the glmnet model without penalizing Sepal.Length
#   Fit = glmnet(x = Logistic$Train_X,
#                y = Logistic$Train_y %>% unlist,
#                family = Logistic$Family,
#                alpha = 0.5,
#                lambda = Logistic$penalty_lambda[40],
#                penalty.factor = Logistic$penalty_factor)


  # We can now proceed to make predictions or extract coefficients as needed



}



























