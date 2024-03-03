Test___Normality___Data.Frame = function(Data, Group_Var=NULL, Response_Var=NULL, outlier_method = "IQR", alpha=0.05){
  # 🟥Group var #####################################################################################################
  if(!is.null(Group_Var) && !is.null(Response_Var)){

    Group = Data[,Group_Var] %>% unlist

    Response = Data[,Response_Var] %>% unlist

    Results = tapply(X = Response, INDEX = Group, function(x){

      Test___Normality___Single.Vector(x.vec = x, outlier_method, alpha)

    })

  # 🟥Non-Group var & Response Var ##################################################################################
  }else if(is.null(Group_Var) && !is.null(Response_Var)){

    Response = Data %>% select(!!Response_Var) %>% unlist() %>% unname()

    Results = Test___Normality___Single.Vector(Response, outlier_method, alpha)

  }



   # 🟥Extract Results ##########################################################################
   Results.df = Results %>% Test___Normality___Data.Frame___Extract.Results()



   # 🟥CLT ##########################################################################
   is_large_sample  = as.numeric(Results.df$Norm_results$N_Obs) >= 30
   n_group = Results.df$Norm_results$N_Obs %>% length

   if(sum(is_large_sample) == n_group){

     Results.df$Norm_results$Norm_Tests = "The Central Limit Theorem"
     Results.df$Norm_results$Norm_p.val = "NA"
     Results.df$Norm_results$is.normal = "Asymptotic"
     Results.df$is.normal = TRUE
   }


 # 🟥Return results ##########################################################################
 return(Results.df)
}



