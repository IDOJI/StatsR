Classification___Multinomial___Results___Export = function(Combined.list, path_Export){
  # save RDS
  saveRDS(Combined.list, file=paste0(path_Export, "/Best_Model_Fitting_Results.RDS"))


  # 1) best hyperparameters
  if(!is.null(Combined.list$Fit$lambdaVals)){
    Best_hyperparameters = data.frame(best_lambda = Combined.list$Fit$lambdaVals, best_alpha = Combined.list$Best_alpha)
    write.csv(Best_hyperparameters, paste0(path_Export, "/", "1.Best_hyperparameters.csv"), row.names=F)
    cat("\n", crayon::green("Exporting"), crayon::red("Best hyperparmeters"), crayon::green("is done!"), "\n")
  }




  # 2) Misclassified subjects
  Misclassified_Subjects = Combined.list$Misclassified_Subjects
  Misclassified_Subjects = tibble::rownames_to_column(Misclassified_Subjects, "Variables")
  write.csv(Misclassified_Subjects, paste0(path_Export, "/", "2.Misclassified_Subjects.csv"), row.names=F)
  cat("\n", crayon::green("Exporting"), crayon::red("Misclassified subjects"), crayon::green("is done!"), "\n")




  # 3) Coefficients
  Coefficients = Combined.list$Fit_Coef
  Coefficients = tibble::rownames_to_column(as.data.frame(Coefficients), "Variables_New")
  write.csv(Coefficients, paste0(path_Export, "/", "3.Coefficients.csv"), row.names=F)
  cat("\n", crayon::green("Exporting"), crayon::red("Coefficients"), crayon::green("is done!"), "\n")




  # 4) Confusion matrix
  Confusion = Combined.list$Confusion_Matrix %>% as.matrix %>% as.data.frame
  Confusion.mat = Confusion %>% spread(key = Actual, value = Freq) %>% column_to_rownames(var = "Predicted") %>% as.data.frame
  write.csv(Confusion.mat, paste0(path_Export, "/", "4.Confusion.mat.csv"), row.names=F)
  cat("\n", crayon::green("Exporting"), crayon::red("Confusion matrix"), crayon::green("is done!"), "\n")




  # 5) Misclassification rate
  Misclassification_Rate = Combined.list$Misclassification_Rate
  write.csv(Misclassification_Rate, paste0(path_Export, "/", "5.Misclassification_Rate.csv"), row.names=F)
  cat("\n", crayon::green("Exporting"), crayon::red("Misclassification rate"), crayon::green("is done!"), "\n")



}
