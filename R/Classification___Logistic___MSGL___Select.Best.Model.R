Classification___Multinomial___MSGL___Select.Best.Model = function(Fit_CV.list, alpha_seq){
  #===========================================================================
  # Extract best error for each CV
  #===========================================================================
  Errors = sapply(Fit_CV.list, function(ith_Fit_CV){
    # ith_Fit_CV = Fit_CV.list[[1]]
    #===========================================================================
    # which best?
    #===========================================================================
    ith_which_best = sglOptim::best_model(ith_Fit_CV)



    #===========================================================================
    # Error
    #===========================================================================
    # Extract Cross validation errors (estimated expected generalization error)
    # Misclassification rate
    Best_Error = sglOptim::Err(ith_Fit_CV)[ith_which_best]

    return(Best_Error)
  })




  #===========================================================================
  # Which error is the lowest?
  #===========================================================================
  which_min_error = which.min(Errors)
  Best_alpha = alpha_seq[which_min_error]
  Best_CV = Fit_CV.list[[which_min_error]]



  #=========================================================================
  # Extract best error for each CV
  #=========================================================================
  # sglOptim::best_model(Best_CV)
  # Best_CV$lambda[[1]][32]
  # fit <- msgl::fit(x, classes, alpha = 0.5, lambda = 0.1)

  return(list(Best_CV = Best_CV, Best_alpha = Best_alpha))
}


