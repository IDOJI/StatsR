Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor___Two.Groups = function(p){
  #=========================================================================
  # Extract Results
  #=========================================================================
  Results = ggstatsplot::extract_stats(p)
  Results.df = Results$subtitle_data



  #=========================================================================
  # Rename
  #=========================================================================
  Results_Renamed.df = Results.df %>% rename(Response:=parameter1) %>% rename(Group:=parameter2)





  #=========================================================================
  # Select Cols
  #=========================================================================
  Results_Selected.df = Results_Renamed.df %>% select(-expression)




  #=========================================================================
  # Relocate cols
  #=========================================================================
  Results_Relocated.df = Results_Selected.df %>%
    relocate(n.obs, .after=Group) %>%
    relocate(statistic, .after=last_col()) %>%
    relocate(p.value, .after=last_col()) %>%
    relocate(Response, .after=Group)




  #=========================================================================
  # Return
  #=========================================================================
  return(Results_Relocated.df)
}
