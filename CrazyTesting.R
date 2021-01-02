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

x1_pred = predict(object = xx11, newdata = x1)
plot(x1$X, x1$Y, pch=20,col="darkgrey")
lines(x1$X, x1$Y,lty=1,lwd=2,col="darkgrey")
lines(x1$X, x1_pred,lty=1,lwd=2,col="blue")

BIC(xx11)

## SPLINE - CUBIC - we already define cuts as three (df = 6)

x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')


x1_pred = predict(object=xx11, newdata=list(X = x1$X),se=TRUE)

fit = x1_pred$fit
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Cubic Spline with Knots 3")
lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")

knotLocaitons = attr(bs(x1$X,df=6),"knots")
for (singleKnot in knotLocaitons) {
  abline(v = as.numeric(singleKnot), lty = 2)
}

abline(v = 40, lty = 2)
abline(v = 55, lty = 6)
abline(v = 59, lty = 4)
geom_vline(xintercept =  15.5)


## SPLINE - NATURAL - we already define cuts as three (df = 4)

x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Vatican', '2020-04-04', '2020-06-01', 'new_cases')


knotLocaitons = attr(bs(x1$X,df=4),"knots")

x1_pred = predict(object=xx11, newdata=list(X = x1$X),se=TRUE)

fit = x1_pred$fit
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Natural Spline with Knots 3")
lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")

knotLocaitons = attr(ns(x1$X, df=4),"knots")
for (singleKnot in knotLocaitons) {
  abline(v = as.numeric(singleKnot), lty = 2)
}

abline(v = as.numeric(c(knotLocaitons)), lty = 5)
as.numeric(c(knotLocaitons))

## SPLINE - SMOOTH - we already define cuts as three (df = CV)


CP(xx11)

xx11$df

xx11$y

install.packages("npreg")
require(npreg)
spline_usingSS = ss(x1$X, y= x1$Y, method = 'BIC', df = xx11$df)

install.packages("santaR")
require(santaR)

BIC_smooth_spline(xx11)


spline_as_NS = (lm(formula=(Y ~ ns(X, df=4)), data=x1))
spline_as_BS = (lm(formula=(Y ~ bs(X, df=xx11$df)), data=x1))

spline_usingSS$y
xx11$y

x1_pred = predict(object=xx11, x = x1$X)
x1_pred_2 = predict(object=spline_usingSS, x = x1$X)

lines(x1$X, x1_pred$y, lty=1, lwd=2, col="blue")
lines(x1$X, x1_pred_2$y, lty=1, lwd=2, col="red")


x1_pred$y == x1_pred_2$y

fit = x1_pred$y
se = x1_pred$se.fit
bands = cbind(fit-2*se,fit+2*se)

plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Smooth Spline with Knots 3")
lines(xx11,lwd=2,col="blue")

smooth


lines(x1$X, fit, lty=1, lwd=2, col="blue")
matlines(x1$X, bands, lty=2, lwd=2, col="blue")



x1  = lm(NewCasesPerCountry~bs(dayCounter,df=6),data=COVID19)
prediction = predict(object=Models.cubic.spline[[i]],newdata=list(dayCounter),se=TRUE)


Models.cubic.spline[[50]]


###########################################################################

testsplit = 80
x1 = get_ModelingData_ByCountryCodeAndDates('local', 'Albania', '2020-04-04', '2020-06-01', 'new_cases')
plot(x1$X, x1$Y, pch=20, col="darkgrey", main="Actual Data")

model_lr = build_LinearRegressionModel(modelData = x1, testsplit)
rss(model_lr)
summary(model_lr)
BIC(model_lr)
AIC(model_lr)

modle_cubic = build_CubicSplineModel(modelData = x1, testsplit)
summary(modle_cubic)
BIC(modle_cubic)
AIC(modle_cubic)

model_natural = build_NaturalSplineModel(modelData = x1, testsplit)
summary(model_natural)
BIC(model_natural)
AIC(model_natural)

model_smooth = build_SmoothSplineModel(modelData = x1, testsplit)
names(model_smooth)
summary(model_smooth)
BIC(model_smooth)
AIC(model_smooth)

model_npreg = ss(x = x1$X, y = x1$Y, df = model_smooth$df)
names(model_npreg)
model_npreg$bic
BIC(model_npreg)
AIC(model_npreg)

model_npreg$y

model_smooth$y



