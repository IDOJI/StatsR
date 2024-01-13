Classification___Logistic___Multinomial = function(Logistic){

  Response_Type = Logistic$Response_Type
  Reponse_Type_Choices = c("Nominal", "Ordinal") %>% tolower
  Response_Type = match.arg(tolower(Response_Type), Reponse_Type_Choices)

  if(Response_Type == "nominal"){

    Results = Classification___Logistic___Multinomial___Nominal(Logistic)

  }else if(Response_Type == "ordinal"){

    Results = Classification___Logistic___Multinomial___Ordinal(Logistic)

  }


  return(Results)
}
