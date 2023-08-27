Test___MeanDiff___Ordinal.Group.Var = function(Data,
                                               Response_Vars,
                                               Group_Var){
  #==================================================================================
  # Test for each response
  #==================================================================================
  Results.list = lapply(seq_along(Response_Vars), function(i){
    if(is.Normal[i]){
      Test___MeanDiff___Ordinal.Group.Var___Parametric()
    }else{
      Test___MeanDiff___Ordinal.Group.Var___Nonparametric(Data, Group_Var, Response_Vars[i])
    }
  })
  names(Results.list) = Response_Vars





  #=============================================================================
  # Extract Significant Results for each Response
  #=============================================================================
  Significant_Results = lapply(Results.list, function(y){
    p.vals = y$p.vals
    signif_index = which(p.vals < 0.05)
    if(signif_index[1] == 1 && length(signif_index)<length(p.vals)){
      return(y[signif_index[2], ])
    }else{
      stop("Check the results")
    }
  })






  #=============================================================================
  # Combining Resulst
  #=============================================================================
  Combined_Results = do.call(rbind, Significant_Results)
  Combined_Results = cbind(Group = Group_Var, Response = Response_Vars, Combined_Results)







  #=============================================================================
  # Post-Hoc : adjust p-val
  #=============================================================================
  # p.adjust(Combined_Results$p.vals, method = "BH")
  # p.adjust(Combined_Results$p.vals, method = "BH")



  return(Results.list)
}

#
# require(graphics)
#
# set.seed(123)
# x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)))
# p <- 2*pnorm(sort(-abs(x)))
#
# round(p, 3)
# round(p.adjust(p), 3)
# round(p.adjust(p, "BH"), 3)
#
# ## or all of them at once (dropping the "fdr" alias):
# p.adjust.M <- p.adjust.methods[p.adjust.methods != "fdr"]
# p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(p, meth))
# p.adj.60 <- sapply(p.adjust.M, function(meth) p.adjust(p, meth, n = 60))
# stopifnot(identical(p.adj[,"none"], p), p.adj <= p.adj.60)
# round(p.adj, 3)
# ## or a bit nicer:
# noquote(apply(p.adj, 2, format.pval, digits = 3))
#
#
# ## and a graphic:
# matplot(p, p.adj, ylab="p.adjust(p, meth)", type = "l", asp = 1, lty = 1:6,
#         main = "P-value adjustments")
# legend(0.7, 0.6, p.adjust.M, col = 1:6, lty = 1:6)
#
# ## Can work with NA's:
# pN <- p; iN <- c(46, 47); pN[iN] <- NA
# pN.a <- sapply(p.adjust.M, function(meth) p.adjust(pN, meth))
# ## The smallest 20 P-values all affected by the NA's :
# round((pN.a / p.adj)[1:20, ] , 4)
# stopifnot()
#











#===============================================================================



#
# # Example p-values
# set.seed(123)
# pvals <- runif(20)  # Generate 20 random p-values between 0 and 1
# adjusted_pvals <- p.adjust(pvals, method = "BH")
# print(adjusted_pvals)
# p.adjust
#
#
#
# set.seed(1234)
# g <- rep(1:5, rep(10,5))
# x <- rnorm(50)
# jonckheere.test(x+0.3*g, g)
# x[1:2] <- mean(x[1:2]) # tied data
# jonckheere.test(x+0.3*g, g)
# jonckheere.test(x+0.3*g, g, nperm=5000)
#








