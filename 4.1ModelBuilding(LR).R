#************************************************************#
#*  Building Linear Regression Models for each country
#************************************************************#

NewCasesPerCountry = NULL
LRModels = NULL
counter = 1
dayCounter = c(1:59)

for (i in 1:50) #50 conuntries
{
  for(j in 1:59) #59 days
  { 
    NewCasesPerCountry[j] = COVID19$new_cases[counter]
    counter=counter+1
  }
  LRModels[[i]] = lm(NewCasesPerCountry ~ dayCounter )
}

summary(LRModels[[3]])