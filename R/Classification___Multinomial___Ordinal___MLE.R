Classification___Multinomial___Oridinal___MLE = function(X_Train,
                                                         y_Train,
                                                         X_Test = NULL,
                                                         y_Test = NULL,
                                                         y_varname,
                                                         standardize = T,
                                                         method = c("MLE", "Elastic"),
                                                         penatly_alpha = NULL,
                                                         penalty_lambda = NULL,
                                                         family = "cumulative",
                                                         link = c("logistic", "probit", "loglog", "cloglog", "cauchit"),
                                                         tuneMethod = "cvMisclass",
                                                         best.model.criterion = "misclass",
                                                         folds,
                                                         AUC_in_Legend = T,
                                                         title = "",
                                                         path_Export,
                                                         ...){
  #=============================================================================
  # Data combining
  #=============================================================================
  y_Train = y_Train %>% data.frame() %>% setNames(y_varname) %>% as_tibble()
  Binded_Data = bind_cols(y_Train, X_Train)





  #=============================================================================
  # Install.pacakges
  #=============================================================================
  install_packages("MASS")



  #=============================================================================
  # Fit proportional odds model
  #=============================================================================
  fit = MASS::polr(SUB___as.formula(y_varname), data = Binded_Data, Hess=TRUE)
  cor(Data$ODI, Data$RAI)
  cor.test(Data$ODI, Data$RAI)
  plot(Data$ODI, Data$RAI)
  correlation::correlation()
  # Plot with regression line
  ggplot(Data, aes(x=ODI, y=RAI)) +
    geom_point() +  # Scatter plot
    geom_smooth(method="lm", se=FALSE, color="red")  # Regression line


  options(contrasts = c("contr.treatment", "contr.poly"))
  house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  house.plr
  summary(house.plr, digits = 3)
  ## slightly worse fit from
  summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
  ## although it is not really appropriate, can fit
  summary(update(house.plr, method = "loglog", Hess = TRUE), digits = 3)
  summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)

  predict(house.plr, housing, type = "probs")
  addterm(house.plr, ~.^2, test = "Chisq")
  house.plr2 <- stepAIC(house.plr, ~.^2)
  house.plr2$anova
  anova(house.plr, house.plr2)

  house.plr <- update(house.plr, Hess=TRUE)
  pr <- profile(house.plr)
  confint(pr)
  plot(pr)
  pairs(pr)

  #=============================================================================
  # return results
  #=============================================================================
  list(fit, summary(fit)) %>% return
}


