Test___MeanDiff___Export.xlsx.Highlight = function(Results_ANOVA, save.path, file.name, export.separately=TRUE, Group_Var_Type){

  if(Group_Var_Type=="Nominal"){
    Test___MeanDiff___Export.xlsx.Highlight___Nominal(Results_ANOVA, save.path, file.name, export.separately)
  }else if(Group_Var_Type=="Ordinal"){
    Export___xlsx___Highlighting(data.df             = Results_ANOVA,
                                 colors.list         = "#ABEBC6",
                                 which_cols.list     = 1:ncol(Results_ANOVA),
                                 coloring_index.list = which(!Results_ANOVA$Significance %in% c("HNS", "NS")),
                                 save.path           = save.path,
                                 file.name           = file.name,
                                 sheet.name          = "ANOVA Results")
  }



}

# if(length(Have_Post.Hoc)!=0){
#   Test___MeanDiff___Export.xlsx.Highlight___With.Post.Hoc(Results.df, save.path, file.name)
# }else{
#   Test___MeanDiff___Export.xlsx.Highlight___Without.Post.Hoc(Results.df, save.path, file.name)
# }
