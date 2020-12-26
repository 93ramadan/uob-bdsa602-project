#************************************************************#
#*  Building Linear Regression Models for each country
#************************************************************#

Models.LR = NULL
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
  Models.LR[[i]] = lm(NewCasesPerCountry ~ dayCounter )
}

summary(Models.LR[[3]])