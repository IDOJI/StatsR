Classification___Multinomial___Results___Cumulative.Probability.Plot = function(fit, Data, x_varname, title_cum.plot = "Proportional Logit Model Curves", path_Export=NULL){
  #=============================================================================
  # Data selection for plotting
  #=============================================================================
  if(length(x_varname)==1){
    Data = Data[,x_varname]
  }else if(length(x_varname)>1){
    stop("x_varname should have one element to plot a Cumulative plot")
  }





  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("ggplot2", "dplyr"))





  #=============================================================================
  # plotting options
  #=============================================================================
  # colors = c("red", "blue", "green", "purple", "yellow")




  #=============================================================================
  # Define the logistic CDF
  #=============================================================================
  logit_cdf = function(x,intercept, beta) {
    exp(intercept + beta * x) / (1 + exp(intercept + beta * x))
  }






  #=============================================================================
  # Extract beta & intercepts
  #=============================================================================
  if(class(fit)=="polr"){
    summary_fit = summary(fit)
    intercepts = summary_fit$coefficients[-1,1] %>% cumsum
    beta = summary_fit$coefficients[1,1]

  }else if(class(fit) == "ordinalNet"){
    summary_fit = fit$coefs %>% as.data.frame
    which_intercepts = grep("Intercept", names(summary_fit))
    intercepts = summary_fit[1,which_intercepts] %>% unlist
    beta = summary_fit[1,-which_intercepts]


  }else{

  }





  #=============================================================================
  # Exporting intercepts information
  #=============================================================================
  levels = y_Test %>% unlist %>% levels
  # if(beta<0){
  #   levels = rev(levels)
  # }

  install_packages("tibble")
  # transition within groups
  transitions <- sapply(1:length(intercepts), function(i) {
    paste(levels[i], " | ", levels[i + 1])
  })
  # Save the dataframe to CSV
  intercepts.df = tibble(Numbering = names(intercepts), Threshold = transitions, Coefficient = intercepts %>% unlist)
  write.csv(intercepts.df, paste0(path_Export, "/0.Transition_threshold_info.csv"), row.names = FALSE)






  #=============================================================================
  # Decide x range
  #=============================================================================
  # estimated-probability-based x values
  # Probability Ranges: Another strategy is to determine x_min and x_max
  # based on the range of x values
  # where the cumulative probability is within a certain range.
  # For instance, if you want to focus on the range where cumulative probabilities lie between 0.01 and 0.99,
  # you could solve for these boundaries:
  solve_for_x = function(p, intercept, beta) {
    log((1 / p) - 1) / (-beta) - intercept/beta
  }
  x_min = min(sapply(intercepts, function(a) solve_for_x(0.01, a, beta)))
  x_max = max(sapply(intercepts, function(a) solve_for_x(0.99, a, beta)))








  #=============================================================================
  # New data based on curve values
  #=============================================================================
  # Curve values
  Curves = lapply(intercepts, function(a){
    logit_cdf(x, a, beta)
  })
  # Curves.df = do.call(cbind, Curves) %>% as_tibble %>% setNames(paste0("Curve", 1:length(intercepts)))
  Curves.df = do.call(cbind, Curves) %>% as_tibble %>% setNames(1:length(intercepts))
  Binded.df = cbind(x, Curves.df)

  # Convert data to long format for plotting
  Long.df = Binded.df  %>% tidyr::gather(key = "Curve", value = "Value", -x)
  # the -x indicates that the x column should be excluded from the gathering (or melting) process.









  #=============================================================================
  # New data based on curve values
  #=============================================================================
  # if (beta < 0) {
  #   ylab_text <- expression("P(Y" > "j)")
  # } else {
  #   ylab_text <- expression("P(Y" <= "j)")
  # }
  ylab_text <- expression("P(Y" <= "j)")
  # Plot the curves
  p = ggplot(Long.df, aes(x = x, y = Value, color = Curve)) +
    geom_line() +
    labs(title = title_cum.plot,
         x = names(Data)) +
    ylab(ylab_text) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
          axis.title = element_text(size = 16, face="bold"),
          axis.text = element_text(size = 14, face="bold"),
          legend.title = element_text(size = 16, face="bold"),
          legend.text = element_text(size = 14),
          axis.title.y = element_text(angle = 360, # Rotate y-axis label
                                      vjust = 0.5))  # Set y-axis label in the middle








  #=============================================================================
  # probabilities to belong to each category
  #=============================================================================
  matplot(Curves.df, type = 'l')












  #=============================================================================
  # export
  #=============================================================================
  if(!is.null(path_Export)){
    ggsave(filename = paste0(path_Export, "/Proportional Logit Model Curves.png"), width = 6, height = 5, plot = p, dpi = 300, bg="white")
  }



  return(p)
}


Polviews <- read.table("http://users.stat.ufl.edu/~aa/cat/data/Polviews.dat",
                       header = TRUE, stringsAsFactors = TRUE)

Polviews

library(VGAM)
# parallel = TRUE imposes proportional odds structure
# 4 intercepts for 5 y categories
fit <-  vglm(cbind(y1,y2,y3,y4,y5) ~ party + gender,
             family = cumulative(parallel = TRUE),
             data = Polviews)
summary(fit)  # same effects for all 4 logits

