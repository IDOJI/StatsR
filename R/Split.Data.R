Split.Dataset = function(X, y=NULL, train_percentage=0.8, seed=1234){
  #===============================================================================
  # Description
  #===============================================================================
  # Splitting dataset keeping percentage of diagnosis groups



  #===============================================================================
  # Packages
  #===============================================================================
  install_packages("caret")



  #===============================================================================
  # Combining
  #===============================================================================
  # Assume X is your feature matrix and y is your response vector
  # Combine X and y into a single data frame
  Combined_Data = cbind(X, y)





  #===============================================================================
  # Train index
  #===============================================================================
  # Create a stratified random sample of indices for the training set
  set.seed(seed)  # for reproducibility
  trainIndex = caret::createDataPartition(Combined_Data$y, p = train_percentage, list = FALSE)





  #===============================================================================
  # Splitting to Train & Test
  #===============================================================================
  # Create the training and test sets
  # Now, trainSet and testSet are your stratified training and test sets
  Train_Set = Combined_Data[trainIndex, ]
  Test_Set = Combined_Data[-trainIndex, ]






  #===============================================================================
  # Splitting to X & y
  #===============================================================================
  # Train
  Train_y = Train_Set$y
  Train_X = Train_Set[, -ncol(Train_Set)]

  # Test
  Test_y = Test_Set$y
  Test_X = Test_Set[, -ncol(Test_Set)]






  #===============================================================================
  # returning
  #===============================================================================
  return(list(Train_Set = list(X = Train_X, y = Train_y),
              Test_Set = list(X = Test_X, y = Test_y)))
}




















