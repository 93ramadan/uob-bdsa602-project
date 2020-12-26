#************************************************************#
#*  Smoothing Splines for each country
#************************************************************#


#Fit a smoothing spline with a degree-of-freedom selected using 
#the cross-validation method for each country

Models.smoothing.spline = NULL
NewCasesPerCountry = NULL
country.name = NULL
counter=1

for (i in 1:total.conutries) 
{
  country.name[i] = COVID19$country[counter]
  for(j in 1:study.period) 
  { 
    NewCasesPerCountry[j] = COVID19$new_cases[counter]
    counter=counter+1
  }
  
  Models.smoothing.spline[[i]] = smooth.spline(x=dayCounter, y=NewCasesPerCountry, cv=TRUE)
  plot(dayCounter,NewCasesPerCountry,xlab="Days",ylab="Number of New Cases",pch=20,col="darkgrey",main= paste("Smoothing Spline of" ,country.name))
  lines(Models.smoothing.spline[[i]],lwd=2,col="blue")
  
  Models.Error$SmoothingSpline[i] = mean((NewCasesPerCountry-Models.smoothing.spline[[i]]$y)^2)
  
}

print(paste("The degree of freedom of smoothing spline: ", round(Models.smoothing.spline[[3]]$df, 0)))
