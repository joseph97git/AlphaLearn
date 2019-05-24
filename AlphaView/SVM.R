getSVMFit <- function(train, var1, var2, kern, deg) {
  # fits svm model given the two input variable parameters
  
  # create formula
  svmFormula <- as.formula(paste("signal ~", var1, "+", var2))
  
  # fit svm model
  svm_fit <- svm(svmFormula, data = train, kernel = kern, degree = deg)
  
  return(svm_fit)
}

getSVMPredict <- function(model, test) {
  # gets predicted data based on the given model
  
  # predict on test data
  pred <- predict(model, test)
  
  # check accuracy
  test_signal <- data.frame("signal" = factor(test$signal))
  rownames(test_signal) <- 1:nrow(test_signal)
  
  return(test_signal)
}

