Classification___Multinomial___Results___Cumulative.Probability.Plot___polr = function(fit, Data, title_cum.plot="Proportional Logit Model Curves"){



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
