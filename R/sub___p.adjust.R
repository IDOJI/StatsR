sub___p.adjust = function(p.values,
                          method = c("bonferroni", "holm", "hochberg", "hommel","BH", "fdr", "BY", "SidakSS", "SidakSD", "ABH","TSBH", "none"),
                          alpha = 0.05,
                          only.return.p.vals=TRUE, ...){
  # 🟥 adjust ##############################################################################
  method = tolower(method)
  if(method %in% c("bonferroni", "holm", "hochberg", "hommel","bh", "fdr", "by", "none")){
    ## 🟧 p.adjust 함수 =======================================================================
    adj.p.values = p.adjust(p.values, method)
  }else if(method == "tsbh"){
    ## 🟧 TSBH =======================================================================
    TS.spot <- which(method == "TSBH")
    TSBHs <- paste("TSBH", alpha, sep = "_")
    newprocs <- append(method, TSBHs, after = TS.spot)
    newprocs <- newprocs[newprocs != "TSBH"]
    adjp <- matrix(0, m, n + a)
    dimnames(adjp) <- list(NULL, c("raw_p.vals", newprocs))
    adjp[, 1] <- spval
    tmp <- spval
    for (i in (m - 1):1) {
      tmp[i] <- min(tmp[i + 1], min((mgood/i) * spval[i],
                                    1, na.rm = TRUE), na.rm = TRUE)
      if (is.na(spval[i]))
        tmp[i] <- NA
    }
    h0.TSBH <- rep(0, length(alpha))
    names(h0.TSBH) <- paste("h0.TSBH", alpha, sep = "_")
    for (i in 1:length(alpha)) {
      h0.TSBH[i] <- mgood - sum(tmp < alpha[i]/(1 + alpha[i]),
                                na.rm = TRUE)
      adjp[, TS.spot + i] <- tmp * h0.TSBH[i]/mgood
    }
  }else{
    stop("Check methods")
  }



  # 🟥 Results ##############################################################################
  result.df = adj.p.values %>%
    cbind(adj.p.values = ., adj.p.values_2 = format(adj.p.values, scientific = FALSE)) %>%
    cbind(., significance = sub___p.vals.signif.stars(adj.p.values, show.NS = T)) %>%
    cbind(alpha, .) %>%
    cbind(p.adj.method = method, .) %>%
    cbind(p.values, .) %>%
    as_tibble() %>%
    mutate(adj.p.values = adj.p.values %>% as.numeric) %>%
    mutate(p.values = p.values %>% as.numeric) %>%
    mutate(alpha = alpha %>% as.numeric)


  if(only.return.p.vals){
    return(result.df$adj.p.values)
  }else{
    return(result.df)
  }
}



# # 🟥 Before Defining functions ######################################################################
# m <- length(raw_p.vals)
# if(na.rm){
#   mgood <- sum(!is.na(raw_p.vals))
# }else{
#   mgood <- m
# }
# n <- length(method)
# a <- length(alpha)
# index <- order(raw_p.vals)
# h0.ABH <- NULL
# h0.TSBH <- NULL
# spval <- raw_p.vals[index]
# adjp <- matrix(0, m, n + 1)
# dimnames(adjp) <- list(NULL, c("raw_p.vals", method))
# adjp[, 1] <- spval





#===========================================================================
# TSBH
#===========================================================================

# #===========================================================================
# # SidakSS
# #===========================================================================
# if (is.element("SidakSS", method))
#   adjp[, "SidakSS"] <- 1 - (1 - spval)^mgood
# if (is.element("SidakSD", method)) {
#   tmp <- spval
#   tmp[1] <- 1 - (1 - spval[1])^mgood
#   for (i in 2:m) tmp[i] <- max(tmp[i - 1], 1 - (1 - spval[i])^(mgood -
#                                                                  i + 1))
#   adjp[, "SidakSD"] <- tmp
# }
# #===========================================================================
# # ABH
# #===========================================================================
# if(is.element("ABH", method)){
#   tmp <- spval
#   h0.m <- rep(0, mgood)
#   for (k in 1:mgood) {
#     h0.m[k] <- (mgood + 1 - k)/(1 - spval[k])
#   }
#
#   grab <- min(which(diff(h0.m, na.rm = TRUE) > 0), na.rm = TRUE)
#   h0.ABH <- ceiling(min(h0.m[grab], mgood))
#   for (i in (m - 1):1) {
#     tmp[i] <- min(tmp[i + 1],
#                   min((mgood/i) * spval[i], 1, na.rm = TRUE),
#                   na.rm = TRUE)
#
#     if(is.na(spval[i])){
#       tmp[i] <- NA
#     }
#
#   }
#   adjp[, "ABH"] <- tmp * h0.ABH/mgood
# }





























  #===========================================================================
  # Results
  #===========================================================================
  # Results = list(adjp = adjp, index = index, h0.ABH = h0.ABH[1], h0.TSBH = h0.TSBH[1:length(alpha)])
  # p.vals.adjusted = Results$adjp[order(Results$index),2] %>% format(scientific = scientific)
  # p.vals.adjusted_Rounded = round(p.vals.adjusted %>% as.numeric, 4)
  #
  # Significance = SUB___P.vals.Signif.Stars(p.vals.adjusted)
  # Result_2 = data.frame(p.adj.method = method,
  #                       p.vals.adj = p.vals.adjusted,
  #                       p.vals.adj.rounded = p.vals.adjusted_Rounded,
  #                       Significance)
  #
  #
  #
  #
  #
  #
  #
  # #===========================================================================
  # # Return results
  # #===========================================================================
  # if(only.return.p.vals){
  #   Emptry___raw_p.vals[] = p.vals.adjusted %>% as.numeric
  #   return(Emptry___raw_p.vals)
  # }else{
  #   return(Result_2)
  # }




# Results =  multtest::mt.rawp2adjp(rawp = Results_ANOVA$p.vals, proc = "TSBH")

#===========================================================================
# Install.packages
#===========================================================================
# install_packages(c("BiocManager", "multtest"))
# BiocManager::install("multtest")







#===========================================================================
# mt.raw2adjp
#===========================================================================
# multtest::mt.rawp2adjp(raw_p.vals, proc=c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD",
#                           "BH", "BY","ABH","TSBH"), alpha = 0.05, na.rm = FALSE)







#===============================================================================
# argument : proc
#===============================================================================
# Bonferroni
# Bonferroni single-step adjusted p-values for strong control of the FWER.
#
# Holm
# Holm (1979) step-down adjusted p-values for strong control of the FWER.
#
# Hochberg
# Hochberg (1988) step-up adjusted p-values for strong control of the FWER (for raw (unadjusted) p-values satisfying the Simes inequality).
#
# SidakSS
# Sidak single-step adjusted p-values for strong control of the FWER (for positive orthant dependent test statistics).
#
# SidakSD
# Sidak step-down adjusted p-values for strong control of the FWER (for positive orthant dependent test statistics).
#
# BH
# Adjusted p-values for the Benjamini & Hochberg (1995) step-up FDR-controlling procedure (independent and positive regression dependent test statistics).
#
# BY
# Adjusted p-values for the Benjamini & Yekutieli (2001) step-up FDR-controlling procedure (general dependency structures).
#
# ABH
# Adjusted p-values for the adaptive Benjamini & Hochberg (2000) step-up FDR-controlling procedure. This method ammends the original step-up procedure using an estimate of the number of true null hypotheses obtained from p-values.
#
# TSBH
# Adjusted p-values for the two-stage Benjamini & Hochberg (2006) step-up FDR-controlling procedure. This method ammends the original step-up procedure using an estimate of the number of true null hypotheses obtained from a first-pass application of "BH". The adjusted p-values are a-dependent, therefore alpha must be set in the function arguments when using this procedure.







#===============================================================================
# Results arguments
#===============================================================================
# adjp
# A matrix of adjusted p-values, with rows corresponding to hypotheses
# and columns to multiple testing procedures.
# Hypotheses are sorted in increasing order of their raw (unadjusted) p-values.
#
# index
# A vector of row indices, between 1 and length(raw_p.vals),
# where rows are sorted according to their raw (unadjusted) p-values.
# To obtain the adjusted p-values in the original data order, use adjp[order(index),].
#
# h0.ABH
# The estimate of the number of true null hypotheses as proposed by Benjamini & Hochberg (2000) used
# when computing adjusted p-values for the "ABH" procedure (see Dudoit et al., 2007).
#
# h0.TSBH
# The estimate (or vector of estimates) of the number of true null hypotheses as proposed by Benjamini et al. (2006) when computing adjusted p-values for the "TSBH" procedure. (see Dudoit et al., 2007).


# This comment appears to be describing a programming context,
# likely in R, related to adjusting p-values for multiple testing. Here's a breakdown of the comment:
#
# 1. `# index`: This is likely the name of a variable that holds indices (row numbers) of another variable.
#
# 2. `# A vector of row indices, between 1 and length(raw_p.vals),
# `: The variable `index` is a vector containing row numbers (indices). These indices correspond to the positions
# in a variable named `raw_p.vals`, which likely holds unadjusted p-values.
#
# 3. `# where rows are sorted according to their raw (unadjusted) p-values.
# `: This suggests that the indices in `index` are not in the original order of the data
# but are sorted based on the size of the unadjusted p-values in ascending order.
#
# 4. `# To obtain the adjusted p-values in the original data order, use adjp[order(index),].
# `: This line is an instruction on how to use the `index` variable.
# It seems that there is another variable named `adjp` which holds the adjusted p-values,
# but these are in the sorted order (based on the raw p-values).
# If you want to get these adjusted p-values in their original data order (the order they appeared in `raw_p.vals`), you should use the R command `adjp[order(index),]`.
#
# In essence, the comment provides guidance on working with sorted p-values
# and how to revert them back to their original order after adjustments.














# ## S2 File: R code for manuscript
# ## "A Comparison of Multiple Testing Adjustment Methods
# ## with Block-Correlation Positively-Dependent Tests"
# ## by Stevens, Masud, and Suyundikov
#
# ## Last updated 29 March 2017; Copyright 2017 Stevens, Masud, and Suyundikov
#
# ## Set working directory for saved files, and local library path
# ## (optional -- commented out here)
# # setwd("C:\\folder")
# # .libPaths("C:\\folder\\R_libs")
#
# # load packages
# library(MASS);library(Biobase);library(multtest);library(qvalue);library(Matrix); library(lattice); library(latticeExtra); library(pfa)
# library(foreach)
#
#
# ######################
# ## Define functions ##
# ######################
#
# # Define functions to get adjusted p-values (only)
# # from multtest, qvalue, and pfa package methods
# getSidakSS <- function(Pvec)
# {
#   temp <- mt.rawp2adjp(Pvec,proc="SidakSS")
#   adjp <- temp$adjp[order(temp$index),2]
#   return(adjp)
# }
# getSidakSD <- function(Pvec)
# {
#   temp <- mt.rawp2adjp(Pvec,proc="SidakSD")
#   adjp <- temp$adjp[order(temp$index),2]
#   return(adjp)
# }
# getABH <- function(Pvec)
# {
#   temp <- mt.rawp2adjp(Pvec,proc="ABH")
#   adjp <- temp$adjp[order(temp$index),2]
#   return(adjp)
# }
# getTSBH <- function(Pvec)
# {
#   temp <- mt.rawp2adjp(Pvec,proc="TSBH")
#   adjp <- temp$adjp[order(temp$index),2]
#   return(adjp)
# }
# getqvalue <- function(Pvec)
# {
#   temp <- try(qvalue(Pvec,pi0.method="bootstrap",lambda=seq(0,.95,.05)),silent=TRUE)
#   if(class(temp)=="try-error")
#   { temp <- try(qvalue(Pvec,pi0.method="smoother",lambda=seq(0,.95,.05)),silent=TRUE)}
#   if(class(temp)=="try-error")
#   { temp <- try(qvalue(Pvec,pi0.method="smoother",lambda=seq(0,max(Pvec),.01)),silent=FALSE)}
#   if(class(temp)=="try-error")
#   { cat('q-value numeric issue for i,j,k: ',c(i,j,k)); return(rep(NA,length(Pvec)))	}
#   return(temp$qvalues)
# }
# getPFA <- function(Zvec,Sigma)
# {
#   RE <- pfa.test(Zvec,Sigma=Sigma,reg="L1",plot="none")
#   adjp <- RE$adjPvalue[order(RE$Pvalue[,2]),1]
#   return(adjp)
# }
#
#
#
# # Define function to take a matrix of raw p-values
# # (row for simulation, column for test (feature)),
# # and return matrix of adjusted p-values (within each simulation).
# # Methods here in same order as in manuscript.
# get.adj.p <- function(P,method,Z,Sigma)
# {
#   if(method=="Bonferroni")
#   {
#     return(apply(P,1,p.adjust,method="bonferroni"))
#   }
#   if(method=="Sidak SS")
#   {
#     return(apply(P,1,getSidakSS))
#   }
#   if(method=="Sidak SD")
#   {
#     return(apply(P,1,getSidakSD))
#   }
#   if(method=="Holm")
#   {
#     return(apply(P,1,p.adjust,method="holm"))
#   }
#   if(method=="Hommel")
#   {
#     return(apply(P,1,p.adjust,method="hommel"))
#   }
#   if(method=="Hochberg")
#   {
#     return(apply(P,1,p.adjust,method="hochberg"))
#   }
#   if(method=="BH")
#   {
#     return(apply(P,1,p.adjust,method="BH"))
#   }
#   if(method=="BY")
#   {
#     return(apply(P,1,p.adjust,method="BY"))
#   }
#   if(method=="ABH")
#   {
#     return(apply(P,1,getABH))
#   }
#   if(method=="TSBH")
#   {
#     return(apply(P,1,getTSBH))
#   }
#   if(method=="q-value")
#   {
#     return(apply(P,1,getqvalue))
#   }
#   if(method=="PFA")
#   {
#     return(apply(Z,1,getPFA,Sigma=Sigma))
#   }
# }
#
#
# # Function to get power for each method
# get.summary <- function(P)
# {
#   # P a matrix of adjusted P-values (like adjP[[kk]]) -- nrow=numTests, ncol=numSims
#   HoFalse <- (mu != 0)
#   Rej <- (P < alpha)     # This is a matrix of logicals (T=rejected Ho; F=failed to reject Ho)
#   # Get power
#   Rej.F <- Rej[HoFalse,]  # --> now subsetted to all tests that have a false Ho
#   pow <- mean(Rej.F) # average over all simulations
#   pow.sd <- sd(Rej.F)
#   # Get FWER
#   Rej.T <- Rej[!HoFalse,] # --> now subsetted to all tests that have a true Ho
#   Type.I.in.sim <- apply(Rej.T,2,max)  # vector over simulations; 0 = no type I errors, 1 = at least one type I error
#   fwer <- mean(Type.I.in.sim)  # prop. of sims with at least one type I error
#   fwer.sd <- sd(Type.I.in.sim)
#   # Get FDR
#   HoTrue.mat <- matrix(!HoFalse,ncol=numSims,nrow=numTests,byrow=FALSE)
#   fdr <- mean(HoTrue.mat[Rej]) # Of the tests that were rejected (Rej==TRUE),
#   # look at what proportion of them corresponded to true nulls
#   # (averaged over all tests and simulations)
#   fdr.sd <- sd(HoTrue.mat[Rej])
#   # Return results
#   res <- c(pow,fwer,fdr, pow.sd,fwer.sd,fdr.sd)
#   names(res) <- c('Power','FWER','FDR','Power.SD','FWER.SD','FDR.SD')
#   return(res)
# }
#
#
# # Function to run simulation
# #   numTests: number of tests (features)
# #   numSims: number of simulations
# #   numDep: a vector of available numbers of dependent tests, out of numTests total
# #   numBlocks: number of blocks (needs to be even, and a factor of all numDep elements)
# #   numFalseNulls: total number of false nulls (needs to be at least half the number of dependent tests)
# #   MHT.methods: names of MHT methods to run (to save time when only a subset are of interest)
# run.sim <- function(numTests, numSims, numDep, numBlocks, numFalseNulls, MHT.methods)
# {
#   # Make arguments globally accessible (to make some function calls cleaner)
#   numTests <<- numTests; numSims <<- numSims; numDep <<- numDep;
#   numBlocks <<- numBlocks; numFalseNulls <<- numFalseNulls; MHT.methods <<- MHT.methods
#
#   # Make holding object, with element [[j]]
#   # giving (for rho level j) a data.frame with columns
#   # as named below
#   result.list <- list()
#   for(j in 1:length(rhovec))
#   {
#     result.list[[j]] <- data.frame(Alevel=NA,numDep=NA,method=NA,
#                                    power=NA,fwer=NA,fdr=NA,
#                                    power.sd=NA,fwer.sd=NA,fdr.sd=NA)
#   }
#
#   # Loop over group sizes
#   for(i in 1:length(numDep))
#   {
#     cat('starting numDep level',i,date(),'\n')
#     blocksize <- numDep[i] / numBlocks
#
#     # Loop over rho values
#     for(j in 1:length(rhovec))
#     {
#       cat('.. starting rho level',j,date(),'\n')
#       rho <- rhovec[j]
#
#       # make dependent block to go on diagonal
#       block <- matrix(rho,nrow=blocksize,ncol=blocksize)
#       diag(block) <- 1
#
#       # make list of blocks to go on diagonal
#       block_list <- list()
#       # first the dependent blocks
#       for(jj in 1:numBlocks)
#       {
#         block_list[[jj]] <- block
#       }
#       # last the independent tests
#       block_list[[jj+1]] <- diag(numTests-numDep[i])
#
#       # Make Sigma matrix
#       SigmaMat <- bdiag(block_list)
#
#       # Loop over A values
#       for(k in 1:length(Avec))
#       {
#         cat('..... starting A level',k,date(),'\n')
#         A <- Avec[k]
#
#         # Make mu vector
#         # - first half of blocks are false nulls
#         # - second half of blocks are true nulls
#         # - remaining block (independent tests) has mostly true nulls, then
#         #   remaining number of false nulls
#         mu <- rep(0,numTests)
#         mu[1:(numDep[i]/2)] <- A
#         remF <- numFalseNulls - (numDep[i]/2)
#         mu[(numTests-remF+1):numTests] <- A
#         mu <<- mu  # needs to be globally accessible
#
#         # Simulate test statistics
#         # -- this matrix has a row for each simulation
#         #    and a column for each test (feature)
#         Zmat <- mvrnorm(numSims, mu=matrix(mu,ncol=1), Sigma=SigmaMat, tol=.1)
#
#         # Get p-values (assume two-sided test)
#         RawP <- 2*(1-pnorm(abs(Zmat)))
#
#         # Get adjusted P-values from all methods to be considered
#         # (in same order here as in MHT.methods)
#         adjP <- list()
#         numMethods <- length(MHT.methods)
#         for(kk in 1:numMethods)
#         {
#           adjP[[kk]] <- get.adj.p(RawP,MHT.methods[kk],Zmat,SigmaMat)
#         }
#         names(adjP) <- MHT.methods
#
#         # Get power/FDR/FWER for each method
#         result.ijk <- lapply(adjP,get.summary)
#         mat.ijk <- as.data.frame(matrix(unlist(result.ijk),ncol=6,byrow=T))
#         colnames(mat.ijk) <- c('power','fwer','fdr', 'power.sd','fwer.sd','fdr.sd')
#         f.ik <- data.frame(Alevel=Avec[k],numDep=numDep[i],method=MHT.methods)
#         frame.ik <- cbind(f.ik,mat.ijk)
#         result.list[[j]] <- rbind(result.list[[j]],frame.ik)
#
#       }
#
#     }
#
#   }
#
#   # Clean up holding objects (remove empty first row)
#   for(j in 1:length(rhovec))
#   {
#     result.list[[j]] <- result.list[[j]][-1,]
#   }
#
#   print(date())
#   return(result.list)
# }
#
#
#
# #####################
# ## Run simulations ##
# #####################
#
# ## set available values of A (magnitude of true differential expression)
# ## and available values of rho (correlation among dependent tests)
# Avec <- c(.5, 1, 2, 3, 4, 5)
# rhovec <- c(0,.2,.4,.6,.8,.99)
# ## set desired overall alpha level
# alpha <- .05
#
# ### Simulation 1 : large number of tests (focus on FDR control)
# # set seed (for reproducibility)
# set.seed(1234)
# # set number of tests (features) and number of simulations
# numTests1 <- 2000
# numSims1 <- 1000
# # set group sizes (available numbers of dependent tests, out of numTests total)
# # and numbers of blocks (needs to be even, and a factor of all numDep elements)
# numDep1 <- c(120,360)
# numBlocks1 <- 6
# # set total number of false nulls
# # -- needs to be more than half the number of dependent tests
# numFalseNulls1 <- 200
# # Set labels for multiplicity correction methods
# MHT.methods1 <- c("BH","BY","ABH","TSBH","q-value","PFA")
# # do it, and save result; this takes about 62.5 hours
# result.list1 <- run.sim(numTests1, numSims1, numDep1, numBlocks1, numFalseNulls1, MHT.methods1)
# save(result.list1,file="result1.rdata")
#
# ### Simulation 2 : smaller number of tests (focus on FWER control)
# # set seed (for reproducibility)
# set.seed(1234)
# # set number of tests (features) and number of simulations
# numTests2 <- 100
# numSims2 <- 1000
# # set group sizes (available numbers of dependent tests, out of numTests total)
# # and numbers of blocks (needs to be even, and a factor of all numDep elements)
# numDep2 <- c(18, 36)
# numBlocks2 <- 6
# # set total number of false nulls
# # -- needs to be more than half the number of dependent tests
# numFalseNulls2 <- 20
# # Set labels for multiplicity correction methods
# MHT.methods2 <- c("Bonferroni","Sidak SS","Sidak SD","Holm","Hommel","Hochberg")
# # do it, and save result; this takes about 7 minutes
# result.list2 <- run.sim(numTests2, numSims2, numDep2, numBlocks2, numFalseNulls2, MHT.methods2)
# save(result.list2,file="result2.rdata")
#
#
#
# ################################################
# ## Load main objects and make summary figures ##
# ################################################
# load("result1.rdata")
# load("result2.rdata")
# rholab <- c('(a)','(b)','(c)','(d)','(e)','(f)')
# pdf(file="output.pdf",width=7,height=8)
# for(j in 1:length(rhovec))
# {
#   frame.fdr <- result.list1[[j]]
#   A.lab <- paste("A =",frame.fdr$Alevel);  rhoval <- rhovec[j]; rhoL <- rholab[j]
#
#   frame.fdr$power.dn <- frame.fdr$power - 2*frame.fdr$power.sd/sqrt(1000); frame.fdr$power.dn[frame.fdr$power.dn<0] <- 0
#   frame.fdr$power.up <- frame.fdr$power + 2*frame.fdr$power.sd/sqrt(1000); frame.fdr$power.up[frame.fdr$power.dn>1] <- 1
#   plot1 <- dotplot(method~power.dn|A.lab, data=frame.fdr, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c("(","("), col=c('blue','red'),   xlim=c(-.05,1.05),       grid=TRUE)
#   plot2 <- dotplot(method~power|A.lab, data=frame.fdr, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(16,17), col=c('blue','red'),   xlim=c(-.05,1.05), panel = function(...){panel.dotplot(...,col.line="transparent")})
#   plot3 <- dotplot(method~power.up|A.lab, data=frame.fdr, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(")",")"), col=c('blue','red'),   xlim=c(-.05,1.05),   panel = function(...){panel.dotplot(...,col.line="transparent")})
#   print(plot1+plot2+plot3)
#
#   frame.fdr$fdr.dn <- frame.fdr$fdr - 2*frame.fdr$fdr.sd/sqrt(1000); frame.fdr$fdr.dn[frame.fdr$fdr.dn<0] <- 0
#   frame.fdr$fdr.up <- frame.fdr$fdr + 2*frame.fdr$fdr.sd/sqrt(1000); frame.fdr$fdr.up[frame.fdr$fdr.dn>1] <- 1
#   plot1 <- dotplot(method~fdr.dn|A.lab, data=frame.fdr, groups=numDep, xlab="Average FDR ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c("(","("), col=c('blue','red'),   xlim=c(-.05,1.05),       grid=TRUE, panel=function(...){panel.refline(v = c(.05),lty=2,lwd=1.5,col='black');panel.dotplot(...)})
#   plot2 <- dotplot(method~fdr|A.lab, data=frame.fdr, groups=numDep, xlab="Average FDR ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(16,17), col=c('blue','red'),   xlim=c(-.05,1.05), panel = function(...){panel.dotplot(...,col.line="transparent")})
#   plot3 <- dotplot(method~fdr.up|A.lab, data=frame.fdr, groups=numDep, xlab="Average FDR ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(")",")"), col=c('blue','red'),   xlim=c(-.05,1.05),   panel = function(...){panel.dotplot(...,col.line="transparent")})
#   print(plot1+plot2+plot3)
#
#
#   frame.fwer <- result.list2[[j]]
#   A.lab <- paste("A =",frame.fwer$Alevel);  rhoval <- rhovec[j]; rhoL <- rholab[j]
#
#   frame.fwer$power.dn <- frame.fwer$power - 2*frame.fwer$power.sd/sqrt(1000); frame.fwer$power.dn[frame.fwer$power.dn<0] <- 0
#   frame.fwer$power.up <- frame.fwer$power + 2*frame.fwer$power.sd/sqrt(1000); frame.fwer$power.up[frame.fwer$power.dn>1] <- 1
#   plot1 <- dotplot(method~power.dn|A.lab, data=frame.fwer, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c("(","("), col=c('blue','red'),   xlim=c(-.05,1.05),       grid=TRUE)
#   plot2 <- dotplot(method~power|A.lab, data=frame.fwer, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(16,17), col=c('blue','red'),   xlim=c(-.05,1.05), panel = function(...){panel.dotplot(...,col.line="transparent")})
#   plot3 <- dotplot(method~power.up|A.lab, data=frame.fwer, groups=numDep, xlab="Average Power ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(")",")"), col=c('blue','red'),   xlim=c(-.05,1.05),   panel = function(...){panel.dotplot(...,col.line="transparent")})
#   print(plot1+plot2+plot3)
#
#   frame.fwer$fwer.dn <- frame.fwer$fwer - 2*frame.fwer$fwer.sd/sqrt(1000); frame.fwer$fwer.dn[frame.fwer$fwer.dn<0] <- 0
#   frame.fwer$fwer.up <- frame.fwer$fwer + 2*frame.fwer$fwer.sd/sqrt(1000); frame.fwer$fwer.up[frame.fwer$fwer.dn>1] <- 1
#   plot1 <- dotplot(method~fwer.dn|A.lab, data=frame.fwer, groups=numDep, xlab="Average FWER ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c("(","("), col=c('blue','red'),   xlim=c(.015,.075),       grid=TRUE)#, panel=function(...){panel.refline(v = c(.05),lty=2,lwd=2,col='black');panel.dotplot(...)})
#   plot2 <- dotplot(method~fwer|A.lab, data=frame.fwer, groups=numDep, xlab="Average FWER ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(16,17), col=c('blue','red'),   xlim=c(.015,.075), panel = function(...){panel.dotplot(...,col.line="transparent")})
#   plot3 <- dotplot(method~fwer.up|A.lab, data=frame.fwer, groups=numDep, xlab="Average FWER ± 2 SEM", layout=c(1,length(Avec)), ylab=NULL,   main=bquote(.(rhoL) ~rho == .(rhoval)),  pch=c(")",")"), col=c('blue','red'),   xlim=c(.015,.075),   panel = function(...){panel.refline(v = c(.05),lty=2,lwd=1.25,col='black');panel.dotplot(...,col.line="transparent")})
#   print(plot1+plot2+plot3)
# }
# dev.off()
#
#
#
# ###########################################
# ###########################################
# ## Everything below here is for S1 File  ##
# ###########################################
# ###########################################
#
#
#
# ################################################
# ## Consider effect of number of tests on PFA  ##
# ################################################
# ## Look at how changing just the number of tests affects PFA's FDR control
# #  - here, use 6 blocks, each 2.5% of total number tests;
# #    3 blocks for false nulls (at A=2 here), 3 blocks for true nulls (A=0),
# #    remaining indep. tests for true nulls and a few (2.5% of total) false nulls
# pfa.check <- function(rho,numsims)
# {
#   numtest.vec <- 40*c(2,3,5,8,10,15,25,50)
#   mean.fdr.vec <- sd.fdr.vec <- rep(NA,length(numtest.vec))
#   A <- 2
#   for(j in 1:length(numtest.vec))
#   {
#     cat("Starting j=",j,date(),'\n')
#     m <- numtest.vec[j]
#     bsize <- m/40
#     block <- matrix(rho,nrow=bsize,ncol=bsize)
#     diag(block) <- 1
#     block_list <- list()
#     for(jj in 1:6)
#     {
#       block_list[[jj]] <- block
#     }
#     block_list[[jj+1]] <- diag(m-6*bsize)
#     SigmaMat <- bdiag(block_list)
#     mu <- c(rep(A,.075*m),rep(0,.075*m),rep(0,.825*m),rep(A,.025*m))
#     set.seed(1234)
#     Z <- mvrnorm(numsims, mu=mu, Sigma=SigmaMat, tol=.1)
#     adjp <- apply(Z,1,getPFA,Sigma=SigmaMat)
#     fdr <- rep(NA,numsims)
#     for(i in 1:numsims)
#     {
#       fdr[i] <- mean((mu==0)[adjp[,i]<.05])
#     }
#     mean.fdr.vec[j] <- mean(fdr,na.rm=TRUE)
#     sd.fdr.vec[j] <- sd(fdr,na.rm=TRUE)
#     if(j==length(numtest.vec)){cat("Completed",date(),'\n')}
#   }
#   mat <- rbind(round(numtest.vec,0),round(mean.fdr.vec,4),round(sd.fdr.vec,4))
#   rownames(mat) <- c("numTests","meanFDR","sdFDR")
#   fr <- as.data.frame(t(mat))
#   dn <- fr$meanFDR-2*fr$sdFDR/sqrt(numsims); dn[dn<0] <- 0
#   up <- fr$meanFDR+2*fr$sdFDR/sqrt(numsims); up[up>1] <- 1
#   fr$dn <- dn; fr$up <- up
#   return(fr)
# }
# fr.plot <- function(fr,rhoL,rhoval,ymin,ymax)
# {
#   plot(fr$numTests,fr$meanFDR,type='l',log='x',xlab='Number of Tests',ylab='PFA method average FDR ± 2 SEM',
#        main=bquote(.(rhoL) ~rho == .(rhoval)),
#        ylim=c(ymin,ymax))
#   for(i in 1:nrow(fr))
#   {
#     lines(x=c(fr$numTests[i],fr$numTests[i]),
#           y=c(fr$dn[i],fr$up[i]), col='orange',lwd=2)
#   }
# }
# # Run for rho=0 and rho=.2; these two lines together take about 2.2 hours total
# fr0 <- pfa.check(rho=0,numsims=100)
# fr2 <- pfa.check(rho=.2,numsims=100)
# save(list=c('fr0','fr2'),file="resultS1.rdata")
# # Make summary figures
# load("resultS1.rdata")
# pdf(file="figS1.pdf",width=6,height=6)
# fr.plot(fr0,'(a)',0,ymin=0.1,ymax=0.5)
# fr.plot(fr2,'(b)',0.2,ymin=0.1,ymax=0.5)
# dev.off()
#
#
#
# #################################################################
# ## Consider simultaneous representation of FDR/FWER and power  ##
# #################################################################
# load("result1.rdata")
# library(RColorBrewer)
# colvec <- brewer.pal(8,"Blues")[-c(1:2)]
# j <- 1
# rhoval <- rhovec[j]
# frame.fdr <- result.list1[[j]]
# frame.fdr$rho <- rhoval
# FR <- frame.fdr
# for(j in 2:length(rhovec))
# {
#   rhoval <- rhovec[j]
#   frame.fdr <- result.list1[[j]]
#   frame.fdr$rho <- rhoval
#   FR <- rbind(FR,frame.fdr)
# }
# ##
# ## FDR vs power:
# ##
# pdf(file="figS1a.pdf",width=5,height=5)
# for(j in 1:length(rhovec))
# {
#   rhoL <- rholab[j]
#   rhoval <- rhovec[j]
#   t <- FR$rho==rhovec[j]
#   FRt <- FR[t,]
#   k <- FRt$Alevel+1
#   k[FRt$Alevel==0.5] <- 1
#   use.col <- colvec[k]
#   use.pch <- rep(NA)
#   use.pch[FRt$method=="ABH"] <- 1
#   use.pch[FRt$method=="BH"] <- 2
#   use.pch[FRt$method=="BY"] <- 3
#   use.pch[FRt$method=="PFA"] <- 4
#   use.pch[FRt$method=="q-value"] <- 5
#   use.pch[FRt$method=="TSBH"] <- 6
#   plot(FRt$fdr,FRt$power,xlab='Average FDR',ylab='Average Power',main=bquote(.(rhoL) ~rho == .(rhoval)),
#        pch=use.pch,col=use.col,cex=1.5)
#   legend('topright',c("ABH","BH","BY","PFA","q-value","TSBH"),pch=1:6,col='black')
# }
# dev.off()
# ##
# ## FDR vs power (zoom in):
# ##
# pdf(file="figS1b.pdf",width=5,height=5)
# for(j in 1:length(rhovec))
# {
#   rhoL <- rholab[j]
#   rhoval <- rhovec[j]
#   t <- FR$rho==rhovec[j]
#   FRt <- FR[t,]
#   k <- FRt$Alevel+1
#   k[FRt$Alevel==0.5] <- 1
#   use.col <- colvec[k]
#   use.pch <- rep(NA)
#   use.pch[FRt$method=="ABH"] <- 1
#   use.pch[FRt$method=="BH"] <- 2
#   use.pch[FRt$method=="BY"] <- 3
#   use.pch[FRt$method=="PFA"] <- 4
#   use.pch[FRt$method=="q-value"] <- 5
#   use.pch[FRt$method=="TSBH"] <- 6
#   plot(FRt$fdr,FRt$power,xlab='Average FDR',ylab='Average Power',main=bquote(.(rhoL) ~rho == .(rhoval)),
#        pch=use.pch,col=use.col,xlim=c(0,0.1),ylim=c(.85,1),cex=1.5)
#   legend('topright',c("ABH","BH","BY","PFA","q-value","TSBH"),pch=1:6,col='black')
# }
# dev.off()
# ##
# ## FWER vs power:
# ##
# load("result2.rdata")
# colvec <- brewer.pal(8,"Blues")[-c(1:2)]
# j <- 1
# rhoval <- rhovec[j]
# frame.fwer <- result.list2[[j]]
# frame.fwer$rho <- rhoval
# FR <- frame.fwer
# for(j in 2:length(rhovec))
# {
#   rhoval <- rhovec[j]
#   frame.fwer <- result.list2[[j]]
#   frame.fwer$rho <- rhoval
#   FR <- rbind(FR,frame.fwer)
# }
# pdf(file="figS1c.pdf",width=5,height=5)
# for(j in 1:length(rhovec))
# {
#   rhoL <- rholab[j]
#   rhoval <- rhovec[j]
#   t <- FR$rho==rhovec[j]
#   FRt <- FR[t,]
#   k <- FRt$Alevel+1
#   k[FRt$Alevel==0.5] <- 1
#   use.col <- colvec[k]
#   use.pch <- rep(NA)
#   use.pch[FRt$method=="Bonferroni"] <- 1
#   use.pch[FRt$method=="Hochberg"] <- 2
#   use.pch[FRt$method=="Holm"] <- 3
#   use.pch[FRt$method=="Hommel"] <- 4
#   use.pch[FRt$method=="Sidak SD"] <- 5
#   use.pch[FRt$method=="Sidak SS"] <- 6
#   plot(FRt$fdr,FRt$power,xlab='Average FDR',ylab='Average Power',main=bquote(.(rhoL) ~rho == .(rhoval)),
#        pch=use.pch,col=use.col,cex=1.5)
#   legend('topright',c("Bonferroni","Hochberg","Holm","Hommel","Sidak SD","Sidak SS"),pch=1:6,col='black')
# }
# dev.off()
#
#
#
#


