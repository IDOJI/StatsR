Test___Normality = function(Data,
                            Group_Var=NULL,
                            Response_Vars=NULL,
                            outlier_method,
                            alpha = 0.05){
  # 🟥 Result list #############################################################################
  results.list = list()



  # 🟥 Decision ############################################################################
  if(is.null(Group_Var) && is.null(Response_Vars)){

    ## 🟧Single vector ===============================================================
    results.list$Norm_Test_Results = Test___Normality___Single.Vector(Data, outlier_method, alpha) %>% list() %>% setNames(Response_Vars)


  }else if(!is.null(Response_Vars)){
    ## 🟧 Use all the other variables as responses
    Response_Vars <- setdiff(names(Data), Group_Var)


    ## 🟧Data.frame with group var =============================================================================
    results.list$Norm_Test_Results = lapply(Response_Vars, function(ith_Response, ...){

      Test___Normality___Data.Frame(Data = Data, Group_Var = Group_Var, Response_Var = ith_Response, outlier_method, alpha = alpha)

   })
    names(results.list$Norm_Test_Results) = Response_Vars


  }




  # 🟥Histogram + Density + QQplot #############################################################################
  results.list$Plots.list = lapply(Response_Vars, function(ith_Response){

    ith_plots.list = list()

    # Combined Group
    ith_plots.list$Hist_Combined = ggplot___histogram(df = Data,
                                                      x = ith_Response,
                                                      group_var = Group_Var,
                                                      group_combined = T,
                                                      density = T,
                                                      same_colors_density = F,
                                                      path_Export = path_save)

    # Each group differently
    ith_plots.list$Hist_EachGroup = ggplot___histogram(df = Data,
                                                       x = ith_Response,
                                                       group_var = Group_Var,
                                                       group_combined = F,
                                                       same_colors_density = F,
                                                       density = T,
                                                       path_Export = path_save)



    # QQ plot
    ith_plots.list$QQplot = ggplot___QQplot(df = Data, x = ith_Response, group_var = Group_Var, path_Export = path_save)

    return(ith_plots.list)
  })






  # 🟥 Combine results #############################################################################
  Results.list = list()
  for(k in 1:length(Response_Vars)){

    Results.list[[k]] = list(Norm_Test_Result = results.list$Norm_Test_Results[[k]], Norm_Plots = results.list$Plots.list[[k]])

  }
  names(Results.list) = Response_Vars





  cat("\n", crayon::green("Testing"), crayon::red("Normality"), crayon::green("is done!"),"\n")
  return(Results.list)

}



