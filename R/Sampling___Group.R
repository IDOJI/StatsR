Sampling___Group = function(data,
                            group_var,
                            group,
                            proportion,
                            sample_size,
                            replace=TRUE,
                            seed=1123){
  # 🟨Install and loading Packages ===============================================================
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
  install_packages("dplyr")




  # 🟨check arguments ===============================================================
  # proportion
  if(sum(proportion)!=1){
    stop("proportion should be summed as 1.")
  }
  # length_group, length_proportion
  len_group = group %>% length
  len_proportion = proportion %>% length
  if(len_group != len_proportion){
    stop("The length of group and proportion should be same.")
  }



  # 🟨sample size ===============================================================
  # The sample size for each group
  each_group_sample_size = sapply(proportion, function(kth_prop){
    round(sample_size * kth_prop)
  }) %>% setNames(group)




  # 🟨subset data by group_var ===============================================================
  Grouped_data.list = lapply(seq_along(group), function(k){

    dplyr::filter(data, !!sym(group_var) == group[k])

  }) %>% setNames(group)



  # 🟨Random sampling for each df by group ===============================================================
  set.seed(seed)
  Sampled_Grouped_data.list = lapply(seq_along(each_group_sample_size), function(k){

    Grouped_data.list[[k]] %>% sample_n(each_group_sample_size[k], replace = replace)

  }) %>% setNames(group)



  Sampled_Grouped_data.list %>% return
}


