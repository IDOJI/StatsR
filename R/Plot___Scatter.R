Plot___Scatter = function(x, y, Data=NULL, regression.line = TRUE){
  #=============================================================================
  # install.pacakge
  #=============================================================================
  install_package("ggplot2")





  #=============================================================================
  # Input data
  #=============================================================================
  if(is.null(Data)){
    p = ggplot(aes(x=!!sym(x), y=!!sym(y)))
  }else{
    p = ggplot(data = Data, aes_string(x=x, y=y))
  }





  #=============================================================================
  # scatter plot
  #=============================================================================
  p = p + geom_point(color="blue")  # Scatter plot






  #=============================================================================
  # letter sizes
  #=============================================================================
  p = p + theme(
    axis.title.x = element_text(size = 16, face="bold"),  # Size for x-axis label
    axis.title.y = element_text(size = 16, face="bold")   # Size for y-axis label
  )





  #=============================================================================
  # Regression line
  #=============================================================================
  if(regression.line){
    p = p + geom_smooth(method="lm", color="red") # Regression line
  }

  return(p)
}
