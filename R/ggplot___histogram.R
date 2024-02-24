ggplot___histogram = function(df,
                              x = NULL,
                              group_var = NULL,
                              group_combined=T,
                              density = T,
                              same_colors_density = F,
                              path_Export,
                              width = 20,
                              height = 5){
  # 🟥 packages #########################################################################
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
  visual = c("ggplot2", "rlang", "RColorBrewer")
  install_packages(visual)




  # 🟥 filename #########################################################################
  file.name = "[Histogram]"
  # Density
  if(density){
    file.name = paste0(file.name, " + Density")
  }
  # Group
  if(group_combined){
    file.name = paste0(file.name, " + CombinedGroup")
  }
  # variables
  file.name = paste(file.name, x, group_var, sep = "___")






  # 🟥 Group #########################################################################
  if(is.null(group_var)){
    ## 🟨 Non Group ===================================================================
    p = ggplot___histogram___Nongroup(df, x, density)


  }else{
    ## 🟨 Group ===================================================================
    p =  ggplot___histogram___Group(df, x, group_var, group_combined, density, same_colors_density)

  }





  # 🟥 Save plot #########################################################################
  ggsave(plot = p, filename = paste0(path_Export, "/", file.name, ".png"), width = 10, height = 7.5, units = "in", dpi = 200, bg = "white")



  # 🟥 Return #########################################################################
  return(p)

}


