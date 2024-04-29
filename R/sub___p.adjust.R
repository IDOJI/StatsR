sub___p.adjust = function(p.values,
                          method = c("bonferroni", "holm", "hochberg", "hommel",
                                     "BH", "fdr", "BY",
                                     "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH",
                                     "none"),
                          alpha = 0.05,
                          only.return.p.vals=TRUE, ...){
  # 🟥 install packages ##############################################################################
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
  install_packages("multtest")




  # 🟥 adjust ##############################################################################
  # method = tolower(method)
  if(method %in% c("bonferroni", "holm", "hochberg", "hommel","bh", "fdr", "by", "none")){

    adj.p.values = p.adjust(p.values, method)

  }else if(method %in% c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH")){

    result = multtest::mt.rawp2adjp(p.values, method)
    adj.p.values = result$adjp[result$index,1]

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



