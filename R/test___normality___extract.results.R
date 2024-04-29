test___normality___extract.results = function(Results, p.adjust.method = "bonferroni"){
  ### what tests
  tests = sapply(Results, FUN=function(ith_results){
    ith_results[[2]] %>% return
  }) %>% unname


  ### pvalues
  p.vals = sapply(Results, FUN=function(ith_results){
    ith_results[[3]] %>% return
  }) %>% unname


  ### normality
  is.normal = sapply(Results, FUN=function(ith_results){
    ith_results[[4]] %>% return
  })


  ### n_obs
  n_obs = sapply(Results, FUN=function(ith_results){
    ith_results[[5]] %>% return
  }) %>% unname



  ### combining
  combined.df = data.frame(Each.Group=names(is.normal), N_Obs=n_obs, Norm_Tests=tests, Norm_p.val=p.vals, is.normal = is.normal) %>% dplyr::as_tibble()
  rownames(combined.df) = NULL


  ### adjust p-vals
  combined_2.df = sub___p.vals.adjust(raw_p.vals = combined.df$Norm_p.val, method = p.adjust.method) %>%
    cbind(combined.df, .)


  ### is normal
  is.normal = sum(!combined_2.df$Significance_2) ==  nrow(combined_2.df)



  return(list(normality=combined_2.df, is.normal=is.normal))
}



