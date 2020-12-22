#************************************************************#
#*  Convert "date" to DATE data type
#************************************************************#
bahrain$date_asdate = as.Date(bahrain$date, "%m/%d/%Y")

bahrain_forward$date_asdate = as.Date(bahrain_forward$date, "%d-%m-%y")
bahrain_forward$day = seq.int(nrow(bahrain_forward))

#************************************************************#
#*  Remove days which have no registered total cases
#************************************************************#
which_casesNotZero = bahrain$total_cases!=0
#* if a day comes in the middle with total cases as 0, will this cause a error in the analysis since this is time-based analysis?
bahrain = bahrain[which_casesNotZero,]