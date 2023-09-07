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
  # beta
  #=============================================================================
  if(length(beta)>1){
    beta = beta[names(beta)==x_varname] %>% unlist
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
  x_min = sapply(intercepts, function(a) solve_for_x(0.01, a, beta)) %>% unlist %>% min()
  x_max = sapply(intercepts, function(a) solve_for_x(0.99, a, beta)) %>% unlist %>% max()
  if(x_min > x_max){
    x = seq(x_max, x_min, by = 0.01)
  }else{
    x = seq(x_min, x_max, by = 0.01)
  }








  #=============================================================================
  # New data based on curve values
  #=============================================================================
  # Curve values
  Curves = lapply(intercepts, function(a, ...){
    logit_cdf(x, a, beta)
  })
  # Curves.df = do.call(cbind, Curves) %>% as_tibble %>% setNames(paste0("Curve", 1:length(intercepts)))
  Curves.df = do.call(cbind, Curves) %>% as_tibble
  if(beta>0){
    # Curves.df <- Curves.df %>% setNames(LETTERS[1:ncol(Curves.df)]) # ABC로 이름을 정하는 경우
    Curves.df <- Curves.df %>% setNames(intercepts.df$Threshold) # 그룹|그룹으로 이름을 정하는 경우
  }else if(beta<0){
    Curves.df = Curves.df %>% setNames(length(intercepts):1)
    stop("beta<0 and make should the labeling of plots' curves")
  }
  Binded.df = cbind(x, Curves.df)

  # Convert data to long format for plotting
  Long.df = Binded.df  %>% tidyr::gather(key = "Curve", value = "Value", -x)
  # the -x indicates that the x column should be excluded from the gathering (or melting) process.

  # Explicitly set the levels of the Curve column:
  Long.df$Curve <- factor(Long.df$Curve, levels = colnames(Curves.df))






  #=============================================================================
  # Colours
  #=============================================================================
  install_packages("RColorBrewer")
  palette_colors <- brewer.pal(n = length(unique(Long.df$Curve)), name = "Set1")
  reversed_colors <- rev(palette_colors)







  #=============================================================================
  # New data based on curve values
  #=============================================================================
  # if (beta < 0) {
  #   ylab_text <- expression("P(Y" > "j)")
  # } else {
  #   ylab_text <- expression("P(Y" <= "j)")
  # }
  ylab_text <- expression("P(Y" <= "j)")

  p_cumulative = ggplot(Long.df, aes(x = x, y = Value, color = Curve)) +
    geom_line(linewidth=1.5) +
    labs(title = title_cum.plot) +
    xlab(x_varname) +
    ylab(ylab_text) +
    scale_color_brewer(palette = "Set2") +
    # scale_color_manual(values = reversed_colors) +  # Use the reversed colors
    theme_minimal() +
    theme(
      axis.ticks.x = element_blank(),   # Remove x-axis ticks
      axis.text.x = element_blank()     # Remove x-axis tick labels
    ) +
    theme(legend.key.size = unit(1, "cm")) +
    theme(plot.title = element_text(size = 0.2, hjust = 0.5, face="bold"),
          axis.title = element_text(size = 12, face="bold"),
          axis.text = element_text(size = 10, face="bold"),
          legend.title = element_text(size = 12, face="bold"),
          legend.text = element_text(size = 8),
          axis.title.y = element_text(angle = 90, # Rotate y-axis label
                                      vjust = 0.5))  # Set y-axis label in the middle

  # Display the plot
  print(p_cumulative)








  #=============================================================================
  # probabilities to belong to each category
  #=============================================================================
  Splitted_Curves.list = apply(Curves.df, 2, function(x){x %>% as.data.frame})
  Computed_Probabilities.list = list()
  for(k in 1:(length(Splitted_Curves.list)+1)){
    if(k==1){
      # P(Y = 1 | X = x)
      Computed_Probabilities.list[[k]] = Splitted_Curves.list[[k]]
    }else if(k == length(Splitted_Curves.list)+1){
      # P(Y = 4 | X = x)
      Computed_Probabilities.list[[k]] = 1 - Splitted_Curves.list[[k-1]]
    }else{
      # P(Y = 2 | X = x) to P(Y = 3 | X = x)
      Computed_Probabilities.list[[k]] = Splitted_Curves.list[[k]] - Splitted_Curves.list[[k-1]]
    }
  }


  # 열 이름을 그룹 숫자로 하는 경우
  # Computed_Probabilities.df = do.call(cbind, Computed_Probabilities.list) %>% setNames(1:length(Computed_Probabilities.list))
  # 열 이름을 그룹 이름으로 하는 경우
  Computed_Probabilities.df = do.call(cbind, Computed_Probabilities.list) %>% setNames(levels)



  # 한 점에서의 확률의 합이 전부 1임을 확인
  # rowSums(Computed_Probabilities.df) %>% table
  Computed_Probabilities.df = cbind(x, Computed_Probabilities.df) %>% as_tibble












  #=============================================================================
  # plotting the probabilities
  #=============================================================================
  # Convert the Computed_Probabilities.df to long format
  Long_Probabilities.df = Computed_Probabilities.df %>%
    tidyr::gather(key = "Curve", value = "Probability", -x)

  Long_Probabilities.df$Curve = factor(Long_Probabilities.df$Curve, levels=levels)


  # Colours
  palette_colors <- brewer.pal(n = length(unique(Long_Probabilities.df$Curve)), name = "Set1")
  reversed_colors <- rev(palette_colors)


  # Plot the computed probabilities
  ylab_text <- expression("P(Y" == "j)")
  p_probabilities <- ggplot(Long_Probabilities.df, aes(x = x, y = Probability, color = Curve)) +
    geom_line(linewidth = 1.5) +
    # labs(title = title_cum.plot,
    #      x = names(Data)) +
    labs(title = title_cum.plot) +
    ylab(ylab_text) +
    xlab(x_varname) +
    scale_color_brewer(palette = "Set2") +
    # scale_color_manual(values = reversed_colors) +  # Use the reversed colors
    theme_minimal() +
    theme(legend.key.size = unit(1, "cm")) +
    theme(
          axis.ticks.x = element_blank(),   # x축 눈금 없애기
          axis.text.x = element_blank()     # x축 눈금 라벨 없애기
          ) +
    theme(plot.title = element_text(size = 0.2, hjust = 0.5, face="bold"),
          axis.title = element_text(size = 12, face="bold"),
          axis.text = element_text(size = 10, face="bold"),
          legend.title = element_text(size = 12, face="bold"),
          legend.text = element_text(size = 8),
          axis.title.y = element_text(angle=90, # Rotate y-axis label
                                      vjust = 0.5))  # Set y-axis label in the middle

  print(p_probabilities)




  #=============================================================================
  # export
  #=============================================================================
  if(!is.null(path_Export)){
    ggsave(filename = paste0(path_Export, "/Proportional Logit Model Curves_Cumulative___", x_varname, ".png"), width = 6, height = 5, plot = p_cumulative, dpi = 300, bg="white")
    ggsave(filename = paste0(path_Export, "/Proportional Logit Model Curves_Density___", x_varname, ".png"), width = 6, height = 5, plot = p_probabilities, dpi = 300, bg="white")
    cat("\n", crayon::green("Exporting"), crayon::yellow("Cumulative"), crayon::green("and"), crayon::yellow("Density"), crayon::green("plots is done!"), "\n")
  }



  return(list(Cumulative = p_cumulative, Density = p_probabilities))
}


