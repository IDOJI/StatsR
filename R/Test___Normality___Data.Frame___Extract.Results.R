Test___Normality___Data.Frame___Extract.Results = function(Results){
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
  is.normal = sum(combined.df$is.normal)==nrow(combined.df)

  return(list(Norm_results=combined.df, is.normal=is.normal))
}



