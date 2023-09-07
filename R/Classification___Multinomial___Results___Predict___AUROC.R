Classification___Multinomial___Results___Predict___AUROC = function(Predicted_Probs, y_Test_unlist, AUC_in_Legend = FALSE, title, path_Export=NULL){
  #=============================================================================
  # pacakges
  #=============================================================================
  install_packages(c("pROC", "ggplot2", "dplyr"), load = TRUE)



  #=============================================================================
  # Computing ROC
  #=============================================================================
  # Categories
  Categories = levels(y_Test_unlist)

  # Computing
  Extracted_ROC.list = lapply(seq_along(Categories), function(i, ...){
    # Binary version of true labels: 1 for current category, 0 for all other categories
    binary_labels = ifelse(y_Test_unlist == Categories[i], 1, 0)


    # Compute ROC
    ith_ROC = roc(binary_labels, Predicted_Probs[,i])


    # AUC & its CI
    ith_AUC = auc(ith_ROC)
    ith_AUC_CI = ci.auc(ith_ROC)



    # Add ROC data to the data frame
    ith_DF = data.frame(TPR = ith_ROC$sensitivities,
                        FPR = 1 - ith_ROC$specificities,
                        Category = Categories[i])

    list(ROC = ith_ROC, AUC = ith_AUC, AUC_CI = ith_AUC_CI, DF = ith_DF) %>% return()
  })








  #=============================================================================
  # Extract results
  #=============================================================================
  ROC = lapply(Extracted_ROC.list, function(y){y[[1]]})
  AUC = lapply(Extracted_ROC.list, function(y){y[[2]]})
  AUC_CI = lapply(Extracted_ROC.list, function(y){y[[3]]})
  DF = lapply(Extracted_ROC.list, function(y){y[[4]]})
  DF = do.call(rbind, DF)

  groups = DF$Category %>% unique
  DF$Category = factor(DF$Category, levels = groups)
  # DF$Category = factor(DF$Category, levels = c("Normal", "Mild OSA", "Moderate OSA", "Severe OSA"))









  #=============================================================================
  # plotting ROC
  #=============================================================================
  if(is.null(title)){
    title = "ROC Curves with AUC values"
  }




  if(AUC_in_Legend){


    # Add AUC values to the category names for the legend
    Categories_with_auc = paste(Categories, sprintf("(%.2f)", unlist(AUC)))

    p = ggplot(DF, aes(x = FPR, y = TPR, color = factor(Category, levels = Categories))) +
      geom_line(linewidth=1.5) +
      scale_color_manual(name = "Category", values = c("blue", "red", "green", "purple"), labels = Categories_with_auc) +
      labs(title = title, x = "1 - Specificity", y = "Sensitivity") +
      theme_minimal() +
      theme(legend.key.size = unit(1, "cm")) +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Adjusting the title properties
            legend.text = element_text(size = 20),  # Adjusting the legend text size
            axis.title.x = element_text(size = 20, face = "bold"),  # Adjusting the x-axis label size and making it bold
            axis.title.y = element_text(size = 20, face = "bold"))  # Adjusting the y-axis label size and making it bold


  }else{


    p = ggplot(DF, aes(x = FPR, y = TPR, color = Category)) +
      geom_line(linewidth=1.5) +
      scale_color_manual(values = scales::hue_pal()(4)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_text(data = data.frame(AUC = unlist(AUC), Category = Categories, x = 0.2, y = c(0.9, 0.8, 0.7, 0.6)),
                aes(x = x, y = y, label = sprintf("AUC: %.3f", AUC), color = Category), hjust = 0) +
      labs(title = title, x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal() +
      theme(legend.key.size = unit(1, "cm")) +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Adjusting the title properties
            legend.text = element_text(size = 20),  # Adjusting the legend text size
            axis.title.x = element_text(size = 20, face = "bold"),  # Adjusting the x-axis label size and making it bold
            axis.title.y = element_text(size = 20, face = "bold"))  # Adjusting the y-axis label size and making it bold



  }







  #=============================================================================
  # Export plot
  #=============================================================================
  if(!is.null(path_Export)){
    ggsave(filename = paste0(path_Export, "/ROC_plot.png"), plot = p, width = 10, height = 8, dpi = 300, bg  = "white")
  }





  #=============================================================================
  # Returning results
  #=============================================================================
  list(ROC = ROC, AUC = AUC, AUC_CI = AUC_CI, Predicted_Probabilities = DF, Plot = p) %>% return()



}
























