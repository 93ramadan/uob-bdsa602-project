#************************************************************#
#*  Convert "date" to DATE data type
#************************************************************#
COVID19$date = as.Date(COVID19$date, "%Y-%m-%d")

#bahrain_forward$date_asdate = as.Date(bahrain_forward$date, "%d-%m-%y")
#bahrain_forward$day = seq.int(nrow(bahrain_forward))

#************************************************************#
#*  Grouping the cases based on the countries
#************************************************************#


NewCasesPerCountry = NULL
counter = 1
study.period = 59
dayCounter = c(1:study.period)
total.conutries = 50

listOfCountries = unique(COVID19$country)




