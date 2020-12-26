#************************************************************#
#*  Validating Linear Regression Models for each country
#************************************************************#

#************************************************************#
#*using train and test split
#************************************************************#

# Split the data into train and test based on 70% and 30%.

study.period = 59
study.period.train = round(study.period * 0.7,0)
study.period.test = study.period - study.period.train
NewCasesPerCountry.train = NULL
NewCasesPerCountry.test = NULL
Models.GLR.prediction = NULL
Models.GLR.MSE = NULL
dayCounter.train = c(1:study.period.train)
dayCounter.test = c((study.period.train+1) :study.period)


for (singleCountry in listOfCountries)
{
  country.name = singleCountry
  country.dataset = COVID19[(COVID19$country == country.name),]
  rownames(country.dataset) <- NULL
  
  # Ensure Dataset is sorted by Date (From earliest to latest)
  # Extract training and testing dataset using position
  country.dataset.train = country.dataset[(1:study.period.train),]
  country.dataset.train$daycounter = dayCounter.train
  country.dataset.train = country.dataset.train[,4:5]
  
  country.dataset.test = country.dataset[((study.period.train+1):study.period),]
  country.dataset.test$daycounter = dayCounter.test
  country.dataset.test = country.dataset.test[,4:5]

  Models.GLR[[singleCountry]] = lm(formula = new_cases ~ daycounter, data = country.dataset.train)
  
  Models.GLR.prediction[[singleCountry]] = predict(object=Models.GLR[[singleCountry]], newdata=country.dataset.test)
  
  Models.GLR.MSE[[singleCountry]] = mean((country.dataset.test$new_cases-Models.GLR.prediction[[singleCountry]])^2)
}


