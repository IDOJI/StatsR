Classification__Multinomial___Results___Predict___ROAUC = function(fit, X_Test, y_Test, AUC_in_Legend = FALSE, path_Export=NULL){
  #=============================================================================
  # pacakges
  #=============================================================================
  install_packages(c("pROC", "ggplot2", "dplyr"), load = TRUE)




  #=============================================================================
  # Prediction
  #=============================================================================
  Predicted_values = predict(fit, newx = X_Test)





  #=============================================================================
  # Computing ROC
  #=============================================================================
  # Categories
  Categories = levels(y_Test)

  # Computing
  Extracted_ROC.list = lapply(seq_along(Categories), function(i, ...){
    # Binary version of true labels: 1 for current category, 0 for all other categories
    binary_labels = ifelse(y_Test == Categories[i], 1, 0)


    # Compute ROC and AUC
    ith_ROC = roc(binary_labels, Predicted_values[,i])
    ith_AUC = auc(ith_ROC)



    # Add ROC data to the data frame
    ith_DF = data.frame(TPR = ith_ROC$sensitivities,
                        FPR = 1 - ith_ROC$specificities,
                        Category = Categories[i])

    list(ROC = ith_ROC, AUC = ith_AUC, DF = ith_DF) %>% return()
  })








  #=============================================================================
  # Extract results
  #=============================================================================
  ROC = lapply(Extracted_ROC.list, function(y){y[[1]]})
  AUC = lapply(Extracted_ROC.list, function(y){y[[2]]})
  DF = lapply(Extracted_ROC.list, function(y){y[[3]]})
  DF = do.call(rbind, DF)
  DF$Category = factor(DF$Category, levels = c("Normal", "Mild OSA", "Moderate OSA", "Severe OSA"))









  #=============================================================================
  # plotting ROC
  #=============================================================================
  if(AUC_in_Legend){


    # Add AUC values to the category names for the legend
    Categories_with_auc <- paste(Categories, sprintf("(AUC = %.2f)", unlist(AUC)))

    p = ggplot(DF, aes(x = `FPR`, y = TPR, color = factor(Category, levels = Categories))) +
      geom_line() +
      scale_color_manual(name = "Category", values = c("blue", "red", "green", "purple"), labels = Categories_with_auc) +
      labs(title = "ROC Curves with AUC values", x = "1 - Specificity", y = "Sensitivity") +
      theme_minimal()


  }else{


    p = ggplot(DF, aes(x = FPR, y = TPR, color = Category)) +
      geom_line() +
      scale_color_manual(values = scales::hue_pal()(4)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_text(data = data.frame(AUC = unlist(AUC), Category = Categories, x = 0.2, y = c(0.9, 0.8, 0.7, 0.6)),
                aes(x = x, y = y, label = sprintf("AUC: %.3f", AUC), color = Category), hjust = 0) +
      labs(title = "ROC Curves with AUC Values", x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal() +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5))  # Adjusting the title properties here



  }








  #=============================================================================
  # Export plot
  #=============================================================================
  if(!is.null(path_Export)){
    ggsave(filename = paste0(path_Export, "/ROC_plot.png"), plot = p, width = 10, height = 8, dpi = 300)
  }





  #=============================================================================
  # Returning results
  #=============================================================================
  list(ROC = ROC, AUC = AUC, Predicted_Probabilities = DF, Plot = p) %>% return()



}
