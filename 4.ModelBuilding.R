#************************************************************#
#*  Build Logistic Growth Model using Existing Data
#************************************************************#

#* Model Formula:
#* Y = (𝝋1) / (1 + exp{-((t-𝝋2)/𝝋3)})
#* 
#* Model Parameters:
#* Y: The number of cases at day "t"
#* t: Day number (i.e. 5 = 5th day)
#* 𝝋1 (Phi1): Upper limit for Y (maximum value)
#* 1/𝝋3 (Phi3): Non-negative value representing Daily Growth Rate
#* 
#* Model Selection (Logistic Growth rather than Exponential Growth Model):
#* 1. It's ability to limit the increase to a value (i.e. cases) rather than increasing to infinite [more realistic]

LogisticGrowthModel = nls(total_cases ~ SSlogis(dayCounter, phi1, phi2, phi3), data = TCTDperDay)
summary(LogisticGrowthModel)

# Extract Model Coefficients
LogisticGrowthModel.CoEf = coef(LogisticGrowthModel)

# Display Model Coefficients
for (CoefficientName in names(LogisticGrowthModel.CoEf)) {
  print(paste("The coefficient of", CoefficientName, "is equal to =", round(LogisticGrowthModel.CoEf[CoefficientName], 3)))
}

# Display Maximum (Upper-Limit)
print(paste("Based on the generated logistic growth model, the maximum number of cases that will be reached is", round(LogisticGrowthModel.CoEf["phi1"], 0)))

# Calculate Daily Growth Rate
DailyGrowthRate = (1/LogisticGrowthModel.CoEf[3])
DailyGrowthRate.Rounded = round(DailyGrowthRate, 3)
print(paste("Based on the generated logistic growth model, the Daily Growth Rate of cases is", DailyGrowthRate.Rounded))

# Calculate Training MSE
TrainingPrediction = LogisticGrowthModel.CoEf[1] / (1+exp(-(TCTDperDay$day-LogisticGrowthModel.CoEf[2])/LogisticGrowthModel.CoEf[3]))
TrainingMSE = mean((TCTDperDay$total_cases - TrainingPrediction)^2)
print(paste("Based on the generated logistic growth model, the Training MSE is", round(TrainingMSE, 2)))
