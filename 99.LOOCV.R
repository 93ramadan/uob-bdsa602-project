#*************************************
#* Using LOOCV
#******************************************
counter = 1
study.period = 59
country.name = NULL
NewCasesPerCountry= NULL
Models.GLR.Error = NULL
Models.GLR.Error.delta = NULL


for (i in 1:total.conutries) 
{
  country.name[i] = COVID19$country[counter]
  for(j in 1:study.period) 
  { 
    NewCasesPerCountry[j] = COVID19$new_cases[counter]
    counter=counter+1
  }
  COVID19.perCountry =  data.frame(dayCounter,NewCasesPerCountry)
  
  Models.GLR[[i]] = glm(NewCasesPerCountry ~ dayCounter, data = COVID19.perCountry)
  Models.GLR.Error[[i]] = cv.glm(data=COVID19.perCountry,glmfit=Models.GLR[[i]]) # NO element of randomness with LOOCV
  Models.GLR.Error.delta[[i]] = round(Models.GLR.Error[[i]]$delta[1],2)
}

#Models.GLR[[8]]
for (i in 1:total.conutries) 
  print(paste("The cross valication error of Linear Regression for new cases in", country.name[i] , "is" ,Models.GLR.Error.delta[[i]]))
