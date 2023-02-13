Summary___Demo = function(data.df, num.demo.col=NULL, char.demo.col=NULL, group.col=NULL, group.as.row=F, path=NULL, file.name=NULL){
  # num.demo.col = c("Age", "AHI")
  # char.demo.col = "Sex"
  # group.col = "AHI_groups"
  # data.df = dataset.df
  #=============================================================================
  if(!is.null(num.demo.col)){
    num.demo_results = Summary___Mean.pm.SD___Groups(data.df, demo.col = num.demo.col, group.col = group.col, group.as.row = group.as.row)
  }else{
    num.demo_results = NULL
  }
  #=============================================================================
  if(!is.null(char.demo.col)){
    char.demo_results = Summary___Prop___Groups(data.df, demo.col=char.demo.col, group.col=group.col, group.as.row=group.as.row)
  }else{
    char.demo_results = NULL
  }
  #=============================================================================
  if(!is.null(num.demo.col)||!is.null(char.demo.col)){
    if(group.as.row){
      summary_results = cbind(num.demo_results, char.demo_results)
    }else{
      summary_results = rbind(num.demo_results, char.demo_results)
    }
  }
  #=============================================================================
  if(!is.null(path) && !is.null(file.name)){
    dir.create(path, showWarnings = F)
    write.xlsx(summary_results, file = paste(path, file.name, sep="/"), overwrite = T, rowNames=T, colNames=T)
  }
  return(summary_results)
}
