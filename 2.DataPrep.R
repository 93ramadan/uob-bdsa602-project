#************************************************************#
#*  Convert "date" to DATE data type
#************************************************************#
COVID19$date = as.Date(COVID19$date, "%Y-%m-%d")

#bahrain_forward$date_asdate = as.Date(bahrain_forward$date, "%d-%m-%y")
#bahrain_forward$day = seq.int(nrow(bahrain_forward))

#************************************************************#
#*  Grouping the cases based on the countries
#************************************************************#
by_country <- COVID19 %>% group_by(country)
by_country
by_country %>% summarise( sum(total_cases))
by_country %>% summarise( length(country))
COVID19$country[59]

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

LRModels




