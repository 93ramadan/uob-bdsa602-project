#************************************************************#
#*  Validating Linear Regression Models for each country
#************************************************************#


for (i in 1:total.conutries) 
{
  country.name = COVID19$country[counter]
  for(j in 1:study.period) 
  { 
    if (j <=  study.period.train)
    {  NewCasesPerCountry.train[j] = COVID19$new_cases[counter]}
    else 
    {  NewCasesPerCountry.test[j-study.period.train] = COVID19$new_cases[counter]}
    counter=counter+1
  }
  #Models.LR[[i]] = lm(NewCasesPerCountry ~ dayCounter )
  train.data =  data.frame(dayCounter.train,NewCasesPerCountry.train)
  Models.GLR[[i]] = glm(NewCasesPerCountry.train ~ dayCounter.train, data = train.data)
  test.data =  data.frame(dayCounter.test,NewCasesPerCountry.test)
  #Models.GLR.prediction[i] = predict(object=Models.GLR[[i]],newdata=test.data)
  
}

Models.GLR.prediction= predict(object=Models.GLR[[50]],newdata=test.data)

