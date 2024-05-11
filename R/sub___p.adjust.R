sub___p.adjust = function(p.values,
                          method = c("bonferroni", "holm", "hochberg", "hommel",
                                     "BH", "fdr", "BY",
                                     "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH",
                                     "none"),
                          alpha = 0.05,
                          only.return.p.vals=TRUE, ...){
  ## 🟧 install packages ##############################################################################
  # install_packages = function(packages, load=TRUE) {
  #   # load : load the packages after installation?
  #   for(pkg in packages) {
  #     if (!require(pkg, character.only = TRUE)) {
  #       install.packages(pkg)
  #     }
  #
  #     if(load){
  #       library(pkg, character.only = TRUE)
  #     }
  #   }
  # }
  # install_packages("multtest")




  ## 🟧 adjust ##############################################################################
  # method = tolower(method)
  if(method %in% c("bonferroni", "holm", "hochberg", "hommel","bh", "fdr", "by", "none")){

    p.adj = p.adjust(p.values, method)

  }else if(method %in% c("Bonferroni",
                         "Holm",
                         "Hochberg",
                         "SidakSS",
                         "SidakSD", "BH", "BY", "ABH", "TSBH")){

    result = mt.rawp2adjp(p.values, method)
    p.adj = result[[1]][result$index,1]

  }else{
    stop("Check methods")
  }



  ## 🟧 Results ##############################################################################
  result.df = cbind(p.adj = p.adj, adj.p.values_2 = format(p.adj, scientific = FALSE)) %>%
    cbind(., p.adj.signif = sub___p.vals.signif.stars(p.adj, show.NS = T)) %>%
    cbind(alpha, .) %>%
    cbind(p.adj.method = method, .) %>%
    cbind(p.values, .) %>%
    as_tibble() %>%
    mutate(p.adj = p.adj %>% as.numeric) %>%
    mutate(p.values = p.values %>% as.numeric) %>%
    mutate(alpha = alpha %>% as.numeric)


  if(only.return.p.vals){
    return(result.df$p.adj)
  }else{
    return(result.df)
  }
}



# 🟥 multtest ===========================================================================================
# ⭐️ Due to the R version problem.
#' Adjusted p-values for simple multiple testing procedures
#'
#' @description This function computes adjusted p-values for simple multiple
#'     testing procedures from a vector of raw (unadjusted) p-values. The
#'     procedures include the Bonferroni, Holm (1979), Hochberg (1988), and
#'     Sidak procedures for strong control of the family-wise Type I error
#'     rate (FWER), and the Benjamini & Hochberg (1995) and Benjamini &
#'     Yekutieli (2001) procedures for (strong) control of the false discovery
#'     rate (FDR). The less conservative adaptive Benjamini & Hochberg (2000)
#'     and two-stage Benjamini & Hochberg (2006) FDR-controlling procedures are
#'     also included. This function is taken from the multtest package. It is
#'     the only function used from this package and is added to this package
#'     wholesale to reduce user installation burden.
#'
#' @usage mt.rawp2adjp(rawp, proc=c("Bonferroni", "Holm", "Hochberg", "SidakSS",
#'     "SidakSD", "BH", "BY","ABH","TSBH"), alpha = 0.05, na.rm = FALSE)
#'
#' @param rawp A vector of raw (unadjusted) p-values for each hypothesis under
#'     consideration. These could be nominal p-values, for example, from
#'     t-tables, or permutation p-values as given in mt.maxT and mt.minP. If
#'     the mt.maxT or mt.minP functions are used, raw p-values should be given
#'     in the original data order, ordered by the index of that data.
#' @param proc A vector of character strings containing the names of the
#'     multiple testing procedures for which adjusted p-values are to be
#'     computed. This vector should include any of the following: "Bonferroni",
#'     "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH".
#' @param alpha A nominal type I error rate, or a vector of error rates, used
#'     for estimating the number of true null hypotheses in the two-stage
#'     Benjamini & Hochberg procedure ("TSBH"). Default is 0.05.
#' @param na.rm An option for handling NA values in a list of raw p-values.
#'     If FALSE, the number of hypotheses considered is the length of the vector
#'      of raw p-values. Otherwise, if TRUE, the number of hypotheses is the
#'      number of raw p-values which were not NAs.
#'
#' @author Sandrine Dudoit, http://www.stat.berkeley.edu/~sandrine,
#' @author Yongchao Ge, yongchao.ge@mssm.edu,
#' @author Houston Gilbert, http://www.stat.berkeley.edu/~houston.
#'
#' @return A list with components: adjp, index, h0.ABH, h0.TSBH. See multtest
#'    package on Bioconductor for details.
mt.rawp2adjp <- function (rawp, proc = c("Bonferroni", "Holm", "Hochberg",
                                         "SidakSS", "SidakSD", "BH", "BY",
                                         "ABH", "TSBH"), alpha = 0.05, na.rm = FALSE)
{
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
  if (is.element("Bonferroni", proc)) {
    tmp <- mgood * spval
    tmp[tmp > 1] <- 1
    adjp[, "Bonferroni"] <- tmp
  }
  if (is.element("Holm", proc)) {
    tmp <- spval
    tmp[1] <- min(mgood * spval[1], 1)
    for (i in 2:m) tmp[i] <- max(tmp[i - 1], min((mgood -
                                                    i + 1) * spval[i], 1))
    adjp[, "Holm"] <- tmp
  }
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
  if (is.element("SidakSS", proc))
    adjp[, "SidakSS"] <- 1 - (1 - spval)^mgood
  if (is.element("SidakSD", proc)) {
    tmp <- spval
    tmp[1] <- 1 - (1 - spval[1])^mgood
    for (i in 2:m) tmp[i] <- max(tmp[i - 1], 1 - (1 - spval[i])^(mgood -
                                                                   i + 1))
    adjp[, "SidakSD"] <- tmp
  }
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
  list(adjp = adjp, index = index, h0.ABH = h0.ABH[1], h0.TSBH = h0.TSBH[1:length(alpha)])
}

