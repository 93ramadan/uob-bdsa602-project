#************************************************************#
#*  Convert "date" to DATE data type
#************************************************************#
COVID19$date = as.Date(COVID19$date, "%Y-%m-%d")

#bahrain_forward$date_asdate = as.Date(bahrain_forward$date, "%d-%m-%y")
#bahrain_forward$day = seq.int(nrow(bahrain_forward))

#************************************************************#
#*  Grouping the cases based on the countries
#************************************************************#
# didnt benifit frm it yet
by_country <- COVID19 %>% group_by(country)
by_country
by_country %>% summarise( sum(total_cases))
by_country %>% summarise( length(country))






