Test___MeanDiff___Export.xlsx.Highlight = function(Results.df, save.path, file.name){
  Have_Post.Hoc = grep("Post.Hoc", names(Results.df))

  if(length(Have_Post.Hoc)!=0){
    Test___MeanDiff___Export.xlsx.Highlight___With.Post.Hoc(Results.df, save.path, file.name)
  }else{
    Test___MeanDiff___Export.xlsx.Highlight___Without.Post.Hoc(Results.df, save.path, file.name)
  }
}
