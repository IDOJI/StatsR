Test___MeanDiff___Nominal.Group.Var = function(Data, Response_Vars, Group_Var){
  #==================================================================================
  # path
  #==================================================================================
  dir.create(save.path, showWarnings = F)






  #==================================================================================
  # file names
  #==================================================================================
  var_group_filename = gsub(pattern = "/", replacement = ".", x = var_group)
  var_responses_filename = gsub(pattern = "/", replacement = ".", x = var_responses)







  #==================================================================================
  # ANOVA results for each groups
  #==================================================================================
  MeanDiff_Multi_Responses.list = lapply(seq_along(var_responses), FUN=function(k, ...){
    ith_results = Test___MeanDiff___Single.Response(df,
                                                    var_group,
                                                    var_response = var_responses[k],
                                                    alpha_Norm,
                                                    alpha_Equal.Var,
                                                    alpha_ANOVA,
                                                    alpha_PostHoc,
                                                    p.adjust.method,
                                                    save.path,
                                                    filename = paste0("[ANOVA_Boxplot] ", '`', var_responses_filename[k],"`"," by `", var_group_filename, "`"),
                                                    export.xlsx = T,
                                                    Boxplot_label.as.p.val = Boxplot_label.as.p.val)
    cat("\n",  crayon::blue("The response variable"), crayon::red(var_responses[k]), crayon::blue("is done!"), "\n")
    return(ith_results)
  })


  install.packages("fdr")


  adaptiveBH <- function(p.values, alpha = 0.05, lambda = 0.5) {
    m <- length(p.values)

    # Step 1: Estimate pi0
    m_star <- sum(p.values > lambda)
    pi0 <- min(1, (m - m_star) / (m * (1 - lambda)))

    # Step 2: Apply Benjamini-Hochberg procedure using p.adjust
    adjusted.p <- p.adjust(p.values, method = "BH") / pi0

    # Limit adjusted p-values to 1
    adjusted.p <- pmin(1, adjusted.p)

    # Determine significant hypotheses
    is.significant <- adjusted.p < alpha

    list(p.adjusted = adjusted.p, significant = is.significant)
  }

  # Example usage:
  alpha <- 0.05
  p <- c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
  result <- adaptiveBH(p, alpha)


  adaptiveBH <- function(p.values, alpha = 0.05, lambda = 0.5) {
    m <- length(p.values)

    # Step 1: Estimate pi0
    m_star <- sum(p.values > lambda)
    pi0 <- min(1, (m - m_star) / (m * (1 - lambda)))

    # Step 2: Apply Benjamini-Hochberg procedure
    p.order <- order(p.values)
    adjusted.p <- p.values[p.order] * m / (1:m) / pi0
    adjusted.p <- cummin(rev(adjusted.p))[rev(order(p.order))]

    # Determine significant hypotheses
    is.significant <- adjusted.p < alpha

    list(p.adjusted = adjusted.p, significant = is.significant)
  }

  alpha <- 0.05
  p <- c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
  result <- adaptiveBH(p, alpha)





  ?multtest::mt.rawp2adjp()
  Sub___Adjust.p.values = function(rawp,
                                   proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                   alpha = 0.05,
                                   na.rm = FALSE){


    # Description
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
    #
    #===========================================================================
    # Install.packages
    #===========================================================================
    install_packages(c("BiocManager", "multtest"))
    # BiocManager::install("multtest")




    #===========================================================================
    # mt.raw2adjp
    #===========================================================================
    # mt.rawp2adjp(rawp, proc=c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD",
    #                           "BH", "BY","ABH","TSBH"), alpha = 0.05, na.rm = FALSE)



    #===========================================================================
    # Defined function
    #===========================================================================
    m <- length(rawp)
    if (na.rm) {
      mgood <- sum(!is.na(rawp))
    }
    else {
      mgood <- m
    }
    n <- length(proc)
    a <- length(alpha)
    index <- order(rawp)
    h0.ABH <- NULL
    h0.TSBH <- NULL
    spval <- rawp[index]
    adjp <- matrix(0, m, n + 1)
    dimnames(adjp) <- list(NULL, c("rawp", proc))
    adjp[, 1] <- spval
    if (is.element("TSBH", proc)) {
      TS.spot <- which(proc == "TSBH")
      TSBHs <- paste("TSBH", alpha, sep = "_")
      newprocs <- append(proc, TSBHs, after = TS.spot)
      newprocs <- newprocs[newprocs != "TSBH"]
      adjp <- matrix(0, m, n + a)
      dimnames(adjp) <- list(NULL, c("rawp", newprocs))
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
        tmp[i] <- min(tmp[i + 1], min((mgood/i) * spval[i],
                                      1, na.rm = TRUE), na.rm = TRUE)
        if (is.na(spval[i]))
          tmp[i] <- NA
      }
      adjp[, "ABH"] <- tmp * h0.ABH/mgood
    }
    #===========================================================================
    # Results
    #===========================================================================
    list(adjp = adjp, index = index, h0.ABH = h0.ABH[1], h0.TSBH = h0.TSBH[1:length(alpha)])
  }
  }

#===============================================================================
# adjp
# A matrix of adjusted p-values, with rows corresponding to hypotheses and columns to multiple testing procedures. Hypotheses are sorted in increasing order of their raw (unadjusted) p-values.
#
# index
# A vector of row indices, between 1 and length(rawp), where rows are sorted according to their raw (unadjusted) p-values. To obtain the adjusted p-values in the original data order, use adjp[order(index),].
#
# h0.ABH
# The estimate of the number of true null hypotheses as proposed by Benjamini & Hochberg (2000) used when computing adjusted p-values for the "ABH" procedure (see Dudoit et al., 2007).
#
# h0.TSBH
# The estimate (or vector of estimates) of the number of true null hypotheses as proposed by Benjamini et al. (2006) when computing adjusted p-values for the "TSBH" procedure. (see Dudoit et al., 2007).





  return(list(Combined_Final.df, Combined_Reporting.df))
}

#
#
# Test___MeanDiff_Multi = function(#################################################
#                                  # data & variabels
#                                  #################################################
#                                  dataset.df,
#                                  variables,
#                                  group_variables,
#                                  #################################################
#                                  # significance level
#                                  #################################################
#                                  norm_alpha=0.05,
#                                  anova_alpha ,
#                                  posthoc_alpha,
#                                  p.adjust.method="bonferroni",
#                                  #################################################
#                                  # path & file names
#                                  #################################################
#                                  group_filenames,
#                                  path,
#                                  file_name = "MeanDiff"){
#
#
#   #==============================================================================
#   # Meandiff results for each group variable
#   #==============================================================================
#
#   MeanDiff_results.list = lapply(group_variables, FUN=function(ith_group_variable, ...){
#     ind = which(ith_group_variable==group_variables)
#
#     ith_filename = group_filenames[ind]
#
#     Final.list = lapply(variables, FUN=function(v, ...){
#       title = paste(v, "by", g,sep=" ")
#       filename = paste(v, "_", f, ".png", sep="")
#       Test___MeanDiff(#################################################
#                       # dataset
#                       #################################################
#                       X                 =    dataset.df,
#                       group             =    ith_group_variable,
#                       variable          =    v,
#                       #################################################
#                       # significance level
#                       #################################################
#                       norm_alpha        =    norm_alpha,
#                       anova_alpha       =    anova_alpha,
#                       posthoc_alpha     =    posthoc_alpha,
#                       p.adjust.method   =    p.adjust.method,
#                       #################################################
#                       # filename
#                       #################################################
#                       title             =    title,
#                       path              =    path,
#                       filename          =    filename)
#     })
#
#
#
#     return(Final_results.df)
#   })
#
#
#
#
#   #################################################
#   # combining results
#   #################################################
#   for(i in 1:length(Final.list)){
#     if(i==1){
#       Final_results.df = Final.list[[i]]
#     }else{
#       Final_results.df = rrbind(Final_results.df, Final.list[[i]])
#     }
#   }
#
#
#   ### combining
#   for(i in 1:length(MeanDiff_results.list)){
#     if(i==1){
#       MeanDiff.df = MeanDiff_results.list[[i]]
#     }else{
#       MeanDiff.df = rrbind(MeanDiff.df, MeanDiff_results.list[[i]])
#     }
#   }
#
#
#
#
#
#   #==============================================================================
#   # Highlighting results & Exporting
#   #==============================================================================
#   ### highlighting
#   which_meandiff_sig = which(MeanDiff.df$MeanDiff_p.val <= anova_alpha)
#   which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
#   coloring_index.list = c(rep(list(which_meandiff_sig),3),
#                           rep(list(which_posthoc_sig),3))
#   colors.list = c(rep(list("#F4FA58"), 3),
#                   rep(list("#FE9A2E"), 3))
#   which_cols.list = which_cols(MeanDiff.df, c("Group Name","Response", "MeanDiff_p.val",
#                                               "Group_1","Group_2","PostHoc_p.val")) %>% as.list
#   coloring_xlsx_cells(data.df = MeanDiff.df,
#                       colors.list = colors.list,
#                       which_cols.list = which_cols.list,
#                       coloring_index.list = coloring_index.list,
#                       save_path = path,
#                       file_name = file_name) %>% suppressWarnings
#
#
#
#   return(MeanDiff.df)
# }


