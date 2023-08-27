Test___Adjust.p.values = function(raw_p.vals,
                                  proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                  alpha = 0.05,
                                  na.rm = FALSE){
    #===========================================================================
    # Defined function
    #===========================================================================
    m <- length(raw_p.vals)
    if (na.rm) {
      mgood <- sum(!is.na(raw_p.vals))
    }
    else {
      mgood <- m
    }
    n <- length(proc)
    a <- length(alpha)
    index <- order(raw_p.vals)
    h0.ABH <- NULL
    h0.TSBH <- NULL
    spval <- raw_p.vals[index]
    adjp <- matrix(0, m, n + 1)
    dimnames(adjp) <- list(NULL, c("raw_p.vals", proc))
    adjp[, 1] <- spval





    #===========================================================================
    # TSBH
    #===========================================================================
    if (is.element("TSBH", proc)) {
      TS.spot <- which(proc == "TSBH")
      TSBHs <- paste("TSBH", alpha, sep = "_")
      newprocs <- append(proc, TSBHs, after = TS.spot)
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
    }
    #===========================================================================
    # Bonferroni
    #===========================================================================
    if (is.element("Bonferroni", proc)) {
      tmp <- mgood * spval
      tmp[tmp > 1] <- 1
      adjp[, "Bonferroni"] <- tmp
    }
    #===========================================================================
    # Holm
    #===========================================================================
    if (is.element("Holm", proc)) {
      tmp <- spval
      tmp[1] <- min(mgood * spval[1], 1)
      for (i in 2:m) tmp[i] <- max(tmp[i - 1], min((mgood -
                                                      i + 1) * spval[i], 1))
      adjp[, "Holm"] <- tmp
    }
    #===========================================================================
    # Hochberg
    #===========================================================================
    if (is.element("Hochberg", proc)) {
      tmp <- spval
      for (i in (m - 1):1) {
        tmp[i] <- min(tmp[i + 1], min((mgood - i + 1) * spval[i],
                                      1, na.rm = TRUE), na.rm = TRUE)
        if (is.na(spval[i]))
          tmp[i] <- NA
      }
      adjp[, "Hochberg"] <- tmp
    }
    #===========================================================================
    # SidakSS
    #===========================================================================
    if (is.element("SidakSS", proc))
      adjp[, "SidakSS"] <- 1 - (1 - spval)^mgood
    if (is.element("SidakSD", proc)) {
      tmp <- spval
      tmp[1] <- 1 - (1 - spval[1])^mgood
      for (i in 2:m) tmp[i] <- max(tmp[i - 1], 1 - (1 - spval[i])^(mgood -
                                                                     i + 1))
      adjp[, "SidakSD"] <- tmp
    }
    #===========================================================================
    # BH
    #===========================================================================
    if (is.element("BH", proc)) {
      tmp <- spval
      for (i in (m - 1):1) {
        tmp[i] <- min(tmp[i + 1], min((mgood/i) * spval[i],
                                      1, na.rm = TRUE), na.rm = TRUE)
        if (is.na(spval[i]))
          tmp[i] <- NA
      }
      adjp[, "BH"] <- tmp
    }
    #===========================================================================
    # BY
    #===========================================================================
    if (is.element("BY", proc)) {
      tmp <- spval
      a <- sum(1/(1:mgood))
      tmp[m] <- min(a * spval[m], 1)
      for (i in (m - 1):1) {
        tmp[i] <- min(tmp[i + 1], min((mgood * a/i) * spval[i],
                                      1, na.rm = TRUE), na.rm = TRUE)
        if (is.na(spval[i]))
          tmp[i] <- NA
      }
      adjp[, "BY"] <- tmp
    }
    #===========================================================================
    # ABH
    #===========================================================================
    if (is.element("ABH", proc)) {
      tmp <- spval
      h0.m <- rep(0, mgood)
      for (k in 1:mgood) {
        h0.m[k] <- (mgood + 1 - k)/(1 - spval[k])
      }
      grab <- min(which(diff(h0.m, na.rm = TRUE) > 0), na.rm = TRUE)
      h0.ABH <- ceiling(min(h0.m[grab], mgood))
      for (i in (m - 1):1) {
        tmp[i] <- min(tmp[i + 1],
                      min((mgood/i) * spval[i], 1, na.rm = TRUE),
                      na.rm = TRUE)

        if(is.na(spval[i])){
          tmp[i] <- NA
        }

      }
      adjp[, "ABH"] <- tmp * h0.ABH/mgood
    }






    #===========================================================================
    # Results
    #===========================================================================
    list(adjp = adjp, index = index, h0.ABH = h0.ABH[1], h0.TSBH = h0.TSBH[1:length(alpha)]) %>% return()

}





#===========================================================================
# Install.packages
#===========================================================================
# install_packages(c("BiocManager", "multtest"))
# BiocManager::install("multtest")







#===========================================================================
# mt.raw2adjp
#===========================================================================
# mt.raw_p.vals2adjp(raw_p.vals, proc=c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD",
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
# A matrix of adjusted p-values, with rows corresponding to hypotheses and columns to multiple testing procedures. Hypotheses are sorted in increasing order of their raw (unadjusted) p-values.
#
# index
# A vector of row indices, between 1 and length(raw_p.vals), where rows are sorted according to their raw (unadjusted) p-values. To obtain the adjusted p-values in the original data order, use adjp[order(index),].
#
# h0.ABH
# The estimate of the number of true null hypotheses as proposed by Benjamini & Hochberg (2000) used when computing adjusted p-values for the "ABH" procedure (see Dudoit et al., 2007).
#
# h0.TSBH
# The estimate (or vector of estimates) of the number of true null hypotheses as proposed by Benjamini et al. (2006) when computing adjusted p-values for the "TSBH" procedure. (see Dudoit et al., 2007).




