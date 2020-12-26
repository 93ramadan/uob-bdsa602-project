#************************************************************#
#*  Cubic Splines for each country
#************************************************************#
Models.cubic.spline = NULL
NewCasesPerCountry = NULL
country.name = NULL
counter=1

for (i in 1:total.conutries) 
{
  country.name = COVID19$country[counter]
  for(j in 1:study.period) 
  { 
    NewCasesPerCountry[j] = COVID19$new_cases[counter]
    counter=counter+1
  }
  #Fit a cubic spline with knots identified at uniform quantiles of the data.
  #The degree-of-freedom for cubic spline is K + 4 = 7 (intercept plus six basis functions)
  Models.cubic.spline[[i]] = lm(NewCasesPerCountry~bs(dayCounter,df=6),data=COVID19)
  prediction = predict(object=Models.cubic.spline[[i]],newdata=list(dayCounter),se=TRUE)
  
  fit = prediction$fit
  se = prediction$se.fit
  CI = cbind(fit-2*se,fit+2*se)
  Models.Error$SplineCubic[i] = mean((NewCasesPerCountry-fit)^2)

  plot(dayCounter,NewCasesPerCountry,xlab="Days",ylab="Number of New Cases",pch=20,col="darkgrey",main=paste("Cubic Spline of" ,country.name))
  lines(dayCounter,fit,lty=1,lwd=2,col="blue") 
  matlines(dayCounter,CI,lty=2,lwd=2,col="blue")
}

#*************************************************************
#* Copy of LM method - Ahmed
#**************************************************************

for (singleCountry in listOfCountries)
{
  country.name = singleCountry
  country.dataset = COVID19[(COVID19$country == country.name),]
  rownames(country.dataset) <- NULL
  
  # Ensure Dataset is sorted by Date (From earliest to latest)
  # Extract training and testing dataset using position
  country.dataset.train = country.dataset[(1:study.period.train),]
  country.dataset.train$daycounter = dayCounter.train
  country.dataset.train = country.dataset.train[,4:5]
  
  country.dataset.test = country.dataset[((study.period.train+1):study.period),]
  country.dataset.test$daycounter = dayCounter.test
  country.dataset.test = country.dataset.test[,4:5]
  
  Models.GLR[[singleCountry]] = lm(formula = new_cases ~ daycounter, data = country.dataset.train)
  
  Models.GLR.prediction[[singleCountry]] = predict(object=Models.GLR[[singleCountry]], newdata=country.dataset.test)
  
  Models.GLR.MSE[[singleCountry]] = mean((country.dataset.test$new_cases-Models.GLR.prediction[[singleCountry]])^2)
}