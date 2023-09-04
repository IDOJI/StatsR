Classification___Multinomial = function(method = c("MLE", "Elastic"), response_type = c("Nominal", "Ordinal")){

  if(response_type == "nominal"){
    Results = Classification___Multinomial___Nominal()
  }else if(response_type == "ordinal"){
    Results = Classification___Multinomial___Ordinal()
  }

  return(Results)
}
