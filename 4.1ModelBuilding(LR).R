#************************************************************#
#*  Building Linear Regression Models for each country
#************************************************************#

Models.LR = NULL
Models.GLR = NULL
NewCasesPerCountry = NULL
country.name = NULL
counter=1

for (i in 1:total.conutries) 
{
  country.name[i] = COVID19$country[counter]
  for(j in 1:study.period) 
  { 
    NewCasesPerCountry[j] = COVID19$new_cases[counter]
    counter=counter+1
  }
  Models.LR[[i]] = lm(NewCasesPerCountry ~ dayCounter )
  Models.GLR[[i]] = glm(NewCasesPerCountry ~ dayCounter )
}

summary(Models.LR[[1]])
summary(Models.GLR[[1]])

fix(COVID19)
