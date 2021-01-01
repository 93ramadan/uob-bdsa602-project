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



### LR
x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')
xx11 = build_LinearRegressionModel(modelData = x1)
x1_pred = predict(object = xx11, newdata = x1)
plot(x1$X, x1$Y, pch=20,col="darkgrey")
lines(x1$X, x1$Y,lty=1,lwd=2,col="darkgrey")
lines(x1$X, x1_pred,lty=1,lwd=2,col="blue")

## SPLINE - CUBIC - we already define cuts as three (df = 6)

x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')
xx11 = build_CubicSplineModel(modelData = x1)
knotLocaitons = attr(bs(x1$X,df=6),"knots")

x1_pred = predict(object=xx11, newdata=list(X = x1$X),se=TRUE)

fit = x1_pred$fit
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Cubic Spline with Knots 3")
lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")


## SPLINE - NATURAL - we already define cuts as three (df = 4)

x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')
xx11 = build_NaturalSplineModel(modelData = x1)
knotLocaitons = attr(bs(x1$X,df=4),"knots")

x1_pred = predict(object=xx11, newdata=list(X = x1$X),se=TRUE)

fit = x1_pred$fit
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Natural Spline with Knots 3")
lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")

## SPLINE - SMOOTH - we already define cuts as three (df = CV)
x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')
xx11 = build_SmoothSplineModel(modelData = x1)


x1_pred = predict(object=xx11, newdata=list(X = x1$X),se=TRUE)

fit = x1_pred$y
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Smooth Spline with Knots 3")
lines(xx11,lwd=2,col="blue")


lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")



x1  = lm(NewCasesPerCountry~bs(dayCounter,df=6),data=COVID19)
prediction = predict(object=Models.cubic.spline[[i]],newdata=list(dayCounter),se=TRUE)


Models.cubic.spline[[50]]






