sub___as.formula = function(y, x=NULL){
  if(length(y)>1){
    stop("'y' should have one element.")
  }



  if(is.null(x)){
    formula_str <- paste(y, "~ .")
    formula_obj <- as.formula(formula_str)

  }else{
    x_form = paste(paste("`", x, "`", sep=""), collapse="+")
    y_form = paste("`", y, "`", sep="")

    formula_str = paste(y_form, x_form, sep="~")
    formula_obj = as.formula(formula_str)
  }


  return(formula_obj)
}
