#************************************************************#
#*  Buliding KNN Regression Models for each country
#************************************************************#

install.packages("smotefamily")
library(smotefamily)
library(class)
library(ISLR)
library(FNN)
library(MASS)

data(Caravan)
attach(Caravan)
Caravan
ScaledPredictors
ScaledPredictors = scale(Caravan[,-86])
index = 1:1000
train.X = ScaledPredictors[-index,]
test.X = ScaledPredictors[ index,]
train.Y = Caravan$Purchase[-index]
test.Y = Caravan$Purchase[ index]
set.seed(1)
# To replicate the same random procedure implemented by R in order to break the tied observations as nearest neighbors.
KNN5 = knn(train.X,test.X,train.Y,k=5)
CM = table(KNN5,test.Y)
CM
train.X = data.frame(train.X)
DATA = SMOTE(X=train.X,target=train.Y,K=1)$data # only the nearest neighbor is considered.
set.seed(1)
# To replicate the same random procedure implemented by R in order to break the tied observations as nearest neighbors.
TRAIN.X = DATA[,-86] # consider the predictors of the balanced training data.
TRAIN.Y = DATA[, 86] # consider the response of the balanced training data.
newKNN5 = knn(TRAIN.X,test.X,TRAIN.Y,k=5)
newCM = table(newKNN5,test.Y)
newCM
#********************************************

COVID19.Scaled = scale(COVID19[,4])
fix(COVID19.Scaled)


#***************************************************
#* try to classify the new cases

#store the data for 1 country
NewCasesPerCountry1 = NULL
for(j in 1:59) #59 days
{ 
  NewCasesPerCountry1[j] = COVID19$new_cases[j]
  #counter=counter+1
}
#create new data table
days = 1:59
country1 = cbind(days, NewCasesPerCountry1)
fix(country1)
is.na(train.cases)

#spliting the date
index = 1:30
train.data = country1[index, ]
test.data  = country1[-index, ]

train.cases  = train.data["NewCasesPerCountry1"]
test.cases   = test.data["NewCasesPerCountry1"]
train.days   = train.data["days"]
test.days    = test.data["days"]


Model.KNN.test = knn.reg(train = train.days, test = test.days, y = train.cases, k = 1)


Model = lm(NewCasesPerCountry ~ dayCounter )
Model.KNN = knn.reg(train= index, test = index.test , y = train.cases, k=5 , algorithm =c("kd_tree", "cover_tree", "brute"))
 
Model.KNN = knn.reg(train=index,   test=index.test , y= data.frame(train.cases),k=5 )
Model.KNN 
summary(Model)
COVID19$new_cases[COVID19$date == '2020-04-04' ]

NewCasesPerCountry1
?knn.reg

index
index.test 
train.cases
test.cases

#******************************************
#*
day = 1:30
plot(train.cases ~ day, xlim=c(0,60)) #adding the scatter for BMI and BMD
lines(seq(31,60), Model.KNN$pred) 
Model.KNN$pred
?plot
