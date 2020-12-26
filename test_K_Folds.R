
folds = createFolds(y=NewCasesPerCountry,k=10)
folds[[1]] # just to see the indices of the first fold
folds[[2]] # just to see the indices of the second fold
cross.validation.error = matrix(NA,nrow=10,ncol=p)
for(k in 1:10)
{
  trainSet = Hitters[-folds[[k]],]
  testSet = Hitters[ folds[[k]],]
  model = regsubsets(dayCounter~.,trainSet,nvmax=p) # identify the the best model of size I using trainSet
  design.matrix = model.matrix(dayCounter~.,data=testSet) # build design matrix for predictions on the remaining fold
  for(i in 1:p)
  {
    coefficients = coef(model,i) # extract the coefficients for the best model of size = i
    names = names(coefficients) # identify the variable names for the best model of size i
    prediction = design.matrix[,names] %*% coefficients # find the predictions for salary variable
    cross.validation.error[k,i] = mean((testSet$dayCounter-prediction)^2) # calculate cv error correspond to fold k
  }
}
E = apply(cross.validation.error,2,mean) # average over columns to get cross-validation error for each best i-model
wh = which.min(E) # specify the size of the best model based on the minimum cross-validation error
coef(BEST,wh)