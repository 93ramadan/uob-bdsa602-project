# Get Data
countryData = get_Data_ByCountryCode('BHR')

# Get Total Days
StudyTimeFrame.Start = as.Date('2020-02-24')
StudyTimeFrame.End = as.Date('2020-07-29')
StudyTimeFrame.TotalDays = as.numeric((StudyTimeFrame.End - StudyTimeFrame.Start), units="days")

# Generate Day Counter
dayCounter = 1 : StudyTimeFrame.TotalDays

# Generate Modeling Data 
modelingData = data.frame(cbind("day" = dayCounter, interestedY = unlist(countryData[interestedY])))
return(modelingData)


countryData[1:StudyTimeFrame.TotalDays,]$new_cases


x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')

head(x1)

colnames(x1)[2]

xx11 = build_LinearRegressionModel(modelFormula = Y~X, modelData = x1)

xx22 = glm(formula=x1$Y ~ x1$X)

x1_pred = predict(object = xx11, newdata = x1)

x1$X

plot(x1$X, x1_pred)

plot(x1$X, x1$Y, pch=20,col="darkgrey")
lines(x1$X, x1$Y,lty=1,lwd=2,col="darkgrey")

lines(x1$X, x1_pred,lty=1,lwd=2,col="blue")

summary(Models.GLR[[50]])

summary(xx11)

summary(xx22)

glm()


sortbydate = get_Data_ByCountryCode('local', 'Bahrain')

sortbydate = rev(arrange(sortbydate, desc(date_asdate)))

sortbydate$date_asdate


StudyTimeFrame.Start = as.Date('2020-04-04')
StudyTimeFrame.End = as.Date('2020-06-01')
StudyTimeFrame.TotalDays = as.numeric((as.Date('2020-06-01')+1 - as.Date('2020-04-04')), units="days")


d123 = get_Data_ByCountryCode('ALB')

d123[d123$date_asdate >= '2020-04-04' & d123$date_asdate <= '2020-06-01', ]

