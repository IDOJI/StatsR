Classification___Multinomial___Results = function(fit,
                                                  Best_alpha=NULL,
                                                  X_Test=NULL,
                                                  y_Test=NULL,
                                                  x_varname,
                                                  y_varname,
                                                  AUC_in_Legend=FALSE,
                                                  title=NULL,
                                                  path_Export=NULL){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  # summary
  Fit_Summary = summary(fit)


  # Coefficients
  Fit_Coef = coef(fit, matrix=TRUE)


  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  Cum_p = Classification___Multinomial___Results___Cumulative.Probability.Plot(fit, X_Test, y_Test)





  #=============================================================================
  # Prediction
  #=============================================================================
  if(!is.null(X_Test) && !is.null(y_Test)){
    Prediction = Classification___Multinomial___Results___Predict(fit, X_Test, y_Test, x_varname, y_varname, AUC_in_Legend, title, path_Export)
  }else{
    Prediction = NULL
  }





  #=============================================================================
  # Combined Results
  #=============================================================================
  Combined.list = c(list(Fit=fit, Fit_Summary = Fit_Summary, Best_alpha = Best_alpha, Fit_Coef = Fit_Coef), Prediction)





  #=============================================================================
  # Export results
  #=============================================================================
  Combined.list$Fit_Summary$coefficients



  #=============================================================================
  # Return
  #=============================================================================
  return(Combined.list)


}





Final_Results.list = c(list(Best_Fit = Best_Fit_Final), Results.list)


if(!is.null(path_Export)){
  # save RDS
  saveRDS(Final_Results.list, file=paste0(path_Export, "/Best_Model_Fitting_Results.RDS"))


  # 1) best hyperparameters
  Best_hyperparameters = data.frame(best_lambda = Final_Results.list$Best_Fit$lambdaVals, best_alpha = Final_Results.list$Best_alpha)
  write.csv(Best_hyperparameters, paste0(path_Export, "/", "Best_hyperparameters.csv"))

  # 2) Misclassified subjects
  Misclassified_Subjects = Final_Results.list$Misclassified_Subjects
  Misclassified_Subjects = tibble::rownames_to_column(Misclassified_Subjects, "Variables")
  write.csv(Misclassified_Subjects, paste0(path_Export, "/", "Misclassified_Subjects.csv"))


  # 3) Coefficients
  Coefficients = Final_Results.list$Fit_Coef
  Coefficients = tibble::rownames_to_column(as.data.frame(Coefficients), "Variables")
  write.csv(Coefficients, paste0(path_Export, "/", "Coefficients.csv"))


  # 4) Confusion matrix
  Confusion = Final_Results.list$Confusion_Matrix %>% as.matrix %>% as.data.frame
  Confusion.mat = Confusion %>% spread(key = Actual, value = Freq) %>% column_to_rownames(var = "Predicted") %>% as.data.frame
  write.csv(Confusion.mat, paste0(path_Export, "/", "Confusion.mat.csv"))


  # 5) Misclassification rate
  Misclassification_Rate = Final_Results.list$Misclassification_Rate
  write.csv(Misclassification_Rate, paste0(path_Export, "/", "Misclassification_Rate.csv"))


}

Classification___Multinomial___Results___Export



Combined.list$Fit %>% class
#===============================================================================
Classification___Multinomial___Results___Cumulative.Probability.Plot = function(fit, Data, title_cum.plot = "Proportional Logit Model Curves"){
  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("ggplot2", "dplyr"))





  #=============================================================================
  # plotting options
  #=============================================================================
  colors = c("red", "blue", "green", "purple", "yellow")




  #=============================================================================
  # Define the logistic CDF
  #=============================================================================
  logit_cdf = function(x,intercept, beta) {
    1 / (1 + exp(- (intercept + beta * x)))
  }



  Classification___Multinomial___Results___Cumulative.Probability.Plot___polr = function(fit, Data, title_cum.plot){
    # Extracting coeffients
    summary_fit = summary(fit)
    beta = summary_fit$coefficients[1,1]
    intercepts = summary_fit$coefficients[-1,1] %>% cumsum



    # Data-based x values
    x_min = (Data %>% unlist %>% min %>% ceiling) +1
    x_max = (Data %>% unlist %>% max %>% ceiling) -1
    x = seq(x_min, x_max, 0.01)



    # Curve values
    Curves = lapply(intercepts, function(a){
      logit_cdf(x, a, beta)
    })
    # Curves.df = do.call(cbind, Curves) %>% as_tibble %>% setNames(paste0("Curve", 1:length(intercepts)))
    Curves.df = do.call(cbind, Curves) %>% as_tibble %>% setNames(names(intercepts))
    Binded.df = cbind(x, Curves.df)



    # Convert data to long format for plotting
    Long.df = Binded.df  %>% tidyr::gather(key = "Curve", value = "Value", -x)
    # the -x indicates that the x column should be excluded from the gathering (or melting) process.




    # Plot the curves
    p = ggplot(Long.df, aes(x = x, y = Value, color = Curve)) +
      geom_line() +
      labs(title = title_cum.plot,
           x = names(Data)) +
      ylab(expression("P(Y" <= "j)")) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
            axis.title = element_text(size = 16, face="bold"),
            axis.text = element_text(size = 14, face="bold"),
            legend.title = element_text(size = 16, face="bold"),
            legend.text = element_text(size = 14))


    return(p)
  }
  #=============================================================================
  # Ordinal___MLE : polr
  #=============================================================================
  if(class(fit)=="polr"){

  }




}





# https://rpubs.com/riazakhan94/logstcdistbasics
cdf=function(x,mu,s){
  k=(x-mu)/s
  return(1/(1+exp(-k)))
}

theData <- tibble(x=seq(-10,12,0.01)) %>%
  mutate(curve0 = cdf(x, 0, 1)) %>%
  mutate(curve2 = cdf(x, 2, 1)) %>%
  mutate(curve4 = cdf(x, 4, 1))

library(ggthemes)  # theme_few
theData %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = curve0), color = colors[3]) +
  geom_line(aes(y = curve2), color = colors[2]) +
  geom_line(aes(y = curve4), color = colors[1]) +
  ggtitle("Figure 6.2") +
  ylab(expression("P(Y" <= "j)")) +
  scale_y_continuous(breaks=c(0,1),
                     labels=c("0", "1")) +
  theme_few() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "segment", x =-5, y = 0.55, xend = 0.0, yend = .55,
           arrow = arrow(length = unit(2, "mm")), color = colors[3]) +
  annotate(geom = "text", x = -8, y = 0.55, label = paste("P(Y <= 3)"),
           parse =  TRUE,hjust = "left", color = colors[3])  +

  annotate(geom = "segment", x =5, y = 0.65, xend = 2.75, yend = .65,
           arrow = arrow(length = unit(2, "mm")) , color = colors[2]) +
  annotate(geom = "text", x = 5, y = 0.65, label = paste("P(Y <= 2)"),
           parse = TRUE, hjust = "left", color = colors[2]) +


  annotate(geom = "segment", x =7.5, y = 0.75, xend = 5.25, yend = .75,
           arrow = arrow(length = unit(2, "mm")) , color = colors[1]) +
  annotate(geom = "text", x = 7.5, y = 0.75,  label = paste("P(Y <= 1)"),
           parse = TRUE, hjust = "left", color = colors[1])
