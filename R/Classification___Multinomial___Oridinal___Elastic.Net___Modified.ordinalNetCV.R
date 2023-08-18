Classification___Multinomial___Oridinal___Elastic.Net___Modified.ordinalNetCV = function (x,
                                                                                          y,
                                                                                          lambdaVals = NULL,
                                                                                          fold.index = NULL,
                                                                                          folds = NULL,
                                                                                          nFolds = 5,
                                                                                          nFoldsCV = 5,
                                                                                          tuneMethod = c("cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"),
                                                                                          printProgress = TRUE,
                                                                                          warn = TRUE,  ...)
{
  #=============================================================================
  # install.packages
  #=============================================================================
  install_packages(packages = "ordinalNet", load = T)





  #=============================================================================
  # Tune Method
  #=============================================================================
  tuneMethod = match.arg(tuneMethod)
  cvID = tuneMethod %in% c("cvLoglik", "cvMisclass", "cvBrier", "cvDevPct")

  if (tuneMethod == "cvLoglik"){
    cvCriterion = "loglik"
  }else if(tuneMethod == "cvMisclass"){
    cvCriterion = "misclass"
  }else if(tuneMethod == "cvBrier"){
    cvCriterion = "brier"
  }else if (tuneMethod == "cvDevPct"){
    cvCriterion = "devPct"
  }





  #=============================================================================
  # Handling Input error
  #=============================================================================
  if (is.matrix(y) && any(rowSums(y) != 1)){
    warning(paste0("Data is split by row for cross validation, but note that ",
                   "y matrix rows have different weights. Be sure this is what you want."))
  }
  if (!is.null(folds) && length(folds) < 2){
    stop(paste0("'folds' should be a list of at least two vectors. ",
                "Each vector should contain indices of a cross validation fold. ",
                "Each index from 1:nrow(x) should be used exactly once."))
  }
  if (!is.null(folds) && !setequal(unlist(folds), 1:nrow(x))){
    stop("'folds' should include each index from 1:nrow(x) exactly once.")
  }




  #=============================================================================
  # y & x
  #=============================================================================
  yMat = if(is.matrix(y)){
    y
  }else{
    yFactorToMatrix(y)
  }
  x = as.matrix()







  #=============================================================================
  # Finding lambdas
  #=============================================================================
  if(printProgress){
    cat("Fitting ordinalNet on full training data\n")
  }
  fit = ordinalNet::ordinalNet(x, y, lambdaVals = lambdaVals, warn = warn, ...)
  if(is.null(lambdaVals)){
    lambdaVals <- fit$lambdaVals
  }






  #=============================================================================
  # Folding
  #=============================================================================
  if (is.null(folds)) {
    n <- nrow(x)
    randIndex <- sample(n)
    folds <- split(randIndex, rep(1:nFolds, length.out = n))
  }
  else {
    nFolds <- length(folds)
  }
  nLambda <- length(lambdaVals)
  loglik <- misclass <- brier <- devPct <- bestLambdaIndex <- rep(NA,
                                                                  nFolds)
  names(loglik) <- names(misclass) <- names(brier) <- names(devPct) <- names(bestLambdaIndex) <- paste0("fold",
                                                                                                        1:nFolds)
  for (i in 1:nFolds) {
    testFold <- folds[[i]]
    xTrain <- x[-testFold, , drop = FALSE]
    xTest <- x[testFold, , drop = FALSE]
    yTrain <- if (is.matrix(y))
      y[-testFold, , drop = FALSE]
    else y[-testFold]
    yMatTest <- yMat[testFold, , drop = FALSE]
    if (printProgress)
      cat("Fitting ordinalNet on fold", i, "of", nFolds,
          "\n")
    if (cvID) {
      fitTrainCV <- ordinalNetTune(xTrain, yTrain, lambdaVals = lambdaVals,
                                   folds = NULL, nFolds = 5, printProgress = FALSE,
                                   warn = FALSE, ...)
      fitTrain <- fitTrainCV$fit
      if (cvCriterion %in% c("loglik", "devPct"))
        wm <- which.max
      if (cvCriterion %in% c("misclass", "brier"))
        wm <- which.min
      bestLambdaIndex[[i]] <- wm(rowMeans(fitTrainCV[[cvCriterion]]))
    }
    else {
      fitTrain <- ordinalNet(xTrain, yTrain, lambdaVals = lambdaVals,
                             warn = FALSE, ...)
      bestLambdaIndex[[i]] <- which.min(fitTrain[[tuneMethod]])
    }
    pHatFull <- predict.ordinalNet(fitTrain, newx = xTest,
                                   type = "response", whichLambda = bestLambdaIndex[[i]])
    pHat <- pHatFull[, -ncol(pHatFull), drop = FALSE]
    loglik[i] <- getLoglik(pHat, yMatTest)
    misclass[i] <- getMisclass(pHat, yMatTest)
    brier[i] <- getBrier(pHat, yMatTest)
    loglikNull <- getLoglikNull(yMatTest)
    devPct[i] <- 1 - loglik[i]/loglikNull
  }



  if(printProgress){
    cat("Done\n")
  }










  #=============================================================================
  # Results
  #=============================================================================
  out = list(loglik = loglik, misclass = misclass, brier = brier,
              devPct = devPct, bestLambdaIndex = bestLambdaIndex, lambdaVals = lambdaVals,
              folds = folds, fit = fit)
  class(out) = "ordinalNetCV"
  return(out)
}

















library(dplyr)

create_age_groups <- function(data, var_name, cut_points) {
  # Create the labels for the age groups
  labels <- c(paste0("0-", cut_points[1]),
              sapply(2:length(cut_points), function(i) {
                paste0(cut_points[i-1], "-", cut_points[i])
              }),
              paste0(tail(cut_points, 1), "+")
  )

  # Create the conditions for the age groups
  conditions <- purrr::map(cut_points, function(x) {
    rlang::expr(!!sym(var_name) <= !!x)
  })

  # Add the default TRUE condition
  conditions <- c(conditions, list(rlang::expr(TRUE)))

  # Combine conditions and labels
  cases <- setNames(conditions, labels)

  # Use the mutate and case_when functions to create the age groups
  data <- data %>%
    mutate(!!sym(paste0(var_name, "_group")) := case_when(
      !!!cases
    ))

  return(data)
}

# Test the function
df <- data.frame(Age = c(15, 20, 30, 40, 50, 60, 70, 80))
cut_points <- c(18, 25, 35, 45, 55, 65, 75)
result <- create_age_groups(df, "Age", cut_points)
print(result)




