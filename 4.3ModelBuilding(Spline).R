#************************************************************#
#*  Splines for each country
#************************************************************#
#
#store the new cases for 1st country

NewCasesPerCountry1 = NULL
for(j in 1:59) #59 days
{ 
  NewCasesPerCountry1[j] = COVID19$new_cases[j]
  #counter=counter+1
}

#create new data of 1st country

days = 1:59
country1 = cbind(days, NewCasesPerCountry1)
country1 = as.data.frame(country1)

POLY = lm(NewCasesPerCountry1~poly(days,4),data=country1)
summary(POLY)

CasesRange = range(NewCasesPerCountry1)
CasesGrid = seq(from=CasesRange[1],to=CasesRange[2])
prediction = predict(object=POLY,newdata=list(days=CasesGrid),se=TRUE)

fit = prediction$fit
se = prediction$se.fit
CI = cbind(fit-2*se,fit+2*se)
head(CI)
plot(days,NewCasesPerCountry1,pch=20,col="darkgrey",main="Degree-Four Polynomial")
lines(CasesGrid,fit,lty=1,lwd=2,col="blue") 
matlines(CasesGrid,CI,lty=2,lwd=2,col="blue")