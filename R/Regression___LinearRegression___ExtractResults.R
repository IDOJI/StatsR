Regression___LinearRegression___ExtractResults = function(fit){
  # 🟥 Install packages ============================================================
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?
    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }
  install_packages("devtools")
  install_github("regbook/regbook", quiet = T)
  library(regbook)




  # 🟥 Results List ============================================================
  Results.list = list()




  # 🟥 Fitting the model==========================================================
  Results.list$Fit = fit




  # 🟥 Summary =================================================================
  Results.list$Summary = summary(fit)




  # 🟥 Coefficients =================================================================
  # list
  Coefficients.list = list()
  # Coefficients
  Coefficients = Results.list$Summary$coefficients %>% as.data.frame
  pvals = Coefficients$`Pr(>|t|)`
  Coefficients = cbind(Variables = rownames(Coefficients),
                       Coefficients,
                       Significance = SUB___P.vals.Signif.Stars(pvals))
  rownames(Coefficients) = NULL
  Coefficients.list$Coefficients = Coefficients
  # Signif vars
  Signif_Vars = Coefficients$Variables[!Coefficients$Significance %in% c("HNS", "NS")]
  Coefficients.list$Signif_Variables = Signif_Vars[Signif_Vars!="(Intercept)"]
  # Combined Results
  Results.list$Coefficients = Coefficients.list


  # 🟥 Diagnosis =================================================================
  # list
  Diagnosis = list()
  ## 🟨 설명력 =================================================================
  # R2
  Diagnosis$R2 = Results.list$Summary$r.squared
  # adj R^2
  Diagnosis$adj_R2 = Results.list$Summary$adj.r.squared



  ## 🟨 ANOVA =================================================================
  # ANOVA Total
  Fstatic = Results.list$Summary$fstatistic %>% as.list
  F_pval = pf(q = Fstatic$value, df1 = Fstatic$numdf, df2 = Fstatic$dendf, lower.tail=F)
  Diagnosis$ANOVA_Total  = c(Fstatic, p_val = F_pval)

  # ANOVA Partial
  Diagnosis$ANOVA_Partial = anova(fit)





  ## 🟨 VIF =================================================================
  n_variables = nrow(Coefficients) - 1
  if(n_variables > 1){
    VIF = regbook::vif(fit)
    Diagnosis$VIF = summary(VIF)
  }



  Results.list$Diagnosis = Diagnosis

  # 🟥 return ==================================================================
  return(Results.list)
}
