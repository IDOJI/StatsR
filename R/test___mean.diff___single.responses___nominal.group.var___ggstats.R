test___mean.diff___single.responses___nominal.group.var___ggstats = function(df,
                                                                             response_var,
                                                                             group_var,
                                                                             alpha_ANOVA,
                                                                             is.normal,
                                                                             is.equal.var,
                                                                             results.subtitle = T,
                                                                             ...){
  # 🟥 Packges ################################################################################
  # install.packages("/Users/Ido/Downloads/ggstatsplot_0.12.0.tar.gz", repos = NULL, type = "source")
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
  install_packages(c("ggthemes", "ggsignif"))





  # 🟥 Return ==========================================================================
  return(list(plot_with_results = plot_with_results, plot_without_results = plot_without_results))

}







