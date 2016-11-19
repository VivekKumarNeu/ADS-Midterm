# Algorithms on the entire model.
install.packages("forecast")
install.packages("devtools")
install.packages("broom")
install.packages("ROCR")
install.packages("FNN")
install.packages('neuralnet')
library(dplyr)
library(tidyr)
library(grid)
library(MASS)
library(neuralnet)
library(FNN)
library(forecast)

###########################################################################

#Setting the working directory-

setwd("/home/sankalp/Documents/ADS/ads_midterm/Data")

#Reading the input data
inputRead <- read.csv("Cleansed.csv")
#inputRead[apply(inputRead[,-1], 1, function(x) !all(x==0)),]

#names(inputRead)

#Selecting only the selected features
inputRead <- subset(inputRead, select = c(KWH,Hour,TemperatureF,Area,DayOfWeek,Month,BaseHourFlag,BuildingID,Meter_Number))

# Dividing the dataset in train and test

read_size <- floor(0.80 * nrow(inputRead))
set.seed(80)
train_data_ind <- sample(seq_len(nrow(inputRead)), size = read_size)
train_data <- inputRead[train_data_ind, ]
test_data <- inputRead[-train_data_ind, ]
train_data[train_data==0]<-0.000001
test_data[test_data==0]<- 0.000001

#Regression on 78 models
for (i in 1:78){
  dataset<- df[[i]]
  names(dataset)
  read_size <- floor(0.80 * nrow(dataset))
  set.seed(80)
  train_data_ind <- sample(seq_len(nrow(dataset)), size = read_size)
  train_data <- dataset[train_data_ind, ]
  test_data <- dataset[-train_data_ind, ]
  train_data[train_data==0]<-0.000001
  test_data[test_data==0]<- 0.000001
  varnames <- c("Hour", "TemperatureF", "Area", "DayOfWeek","Month","BaseHourFlag")
  modelfits <- vector(length(varnames), mode = "list")
  names(modelfits) <- varnames
  names(train_data)
  modelfits[[i]]<- lm(KWH~Hour+TemperatureF+Area+DayOfWeek+Month,data = train_data)
  summary(modelfits[[i]])
  library(forecast)
  pred = predict(modelfits[[i]], test_data)
  accuracy_pred=accuracy(pred, test_data$KWH)
  x <- list(accuracy_pred)
  print(x)
  summary(modelfits[[i]])
}

# Neural Network on 78 models

trainingInput= as.data.frame(runif(50,min=0,max=100))
trainingOutput= sqrt(trainingInput)
print(trainingOutput)
trainingData<-cbind(trainingInput,trainingOutput)
na.omit(df[[1]])
modelfits <- vector(mode = "list")

for(i in 1:78){
  modelfits[[i]]<-neuralnet(KWH~Hour+TemperatureF+Area+DayOfWeek+Month,data=df[[i]],hidden=2,threshold=0.01)
  print(modelfits[[i]])
  plot(modelfits[[i]])
  modelfits[[i]]$result.matrix
}

# Knn on 78 data sets

for(i in 1:78){
  dataset=df[[i]]
  read_size <- floor(0.80 * nrow(dataset))
  set.seed(80)
  train_data_ind <- sample(seq_len(nrow(df[[i]])), size = read_size)
  train_data <- dataset[train_data_ind, ]
  test_data <- dataset[-train_data_ind, ]
  train_data[train_data==0]<-0.000001
  test_data[test_data==0]<- 0.000001
  modelfit[[i]] <- knn.reg(train=train_data,y=train_data$KWH,test_data, k = 3)
  print(modelfit)
  test_data$KWH
  head(test_data$KWH)
  error=as.double(modelfit[[i]]$KWH)-test_data$KWH
  accuracy(train_data$KWH,test_data$KWH)
}

print(accuracy)

# Regression on full dataset

varnames <- c("Hour", "TemperatureF", "Area", "DayOfWeek","Month","BaseHourFlag")
modelfit <- vector(length(varnames), mode = "list")
names(modelfit) <- varnames
names(train_data)
modelfit<- lm(KWH~Hour+TemperatureF+Area+DayOfWeek+Month,data = train_data)
summary(modelfit)
pred = predict(modelfit, test_data)
accuracy_pred=accuracy(pred, test_data$KWH)
print(accuracy_pred)
summary(modelfit)

#Finding error
print(modelfits[[2]])
print(test_data[2]$KWH)
error.reg = (modelfits[[2]]$KWH - test_data$KWH)
print(error.nnet)

mape <- function(error)  {
  mean(abs(error/test_data$KWH) * 100)
}

mae <- function(error)
{
  mean(abs(error))
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#calculating mean square value
rmse(error.nnet)
rms <- c("RMS", rmse(error.reg))
mae(error.nnet)
ma <- c("MAE", mae(error.reg))
mape(error.nnet)
map <- c("MAPE", mape(error.reg))

# Neural Network on entire dataset
modelfit<-neuralnet(KWH~Hour+TemperatureF+Area+DayOfWeek+Month,data=inputRead,hidden=2,threshold=0.01)
print(modelfit)
modelfit$result.matrix

#Finding error
print(modelfit)
print(test_data$KWH)
error.nnet = (modelfits$KWH - test_data$KWH)
print(error.nnet)

mape <- function(error)  {
  mean(abs(error/test_data$KWH) * 100)
}

mae <- function(error)
{
  mean(abs(error))
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#calculating mean square value
rmse(error.nnet)
rms <- c("RMS", rmse(error.nnet))
mae(error.nnet)
ma <- c("MAE", mae(error.nnet))
mape(error.nnet)
map <- c("MAPE", mape(error.nnet))

# KNN on entire dataset
print(train_data)
modelfit <- knn.reg(train=train_data,y=train_data$KWH,test_data, k = 3)
test_data$KWH
head(test_data$KWH)
error=as.double(modelfit$KWH)-test_data$KWH
accuracy(train_data$KWH,test_data$KWH)
summary(modelfit)

#Finding error
error.knn = (modelfit$KWH - test_data$KWH)
print(error.nnet)

mape <- function(error)  {
  mean(abs(error/test_data$KWH) * 100)
}

mae <- function(error)
{
  mean(abs(error))
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#calculating mean square value
rmse(error.nnet)
rms <- c("RMS", rmse(error.knn ))
mae(error.nnet)
ma <- c("MAE", mae(error.knn ))
mape(error.nnet)
map <- c("MAPE", mape(error.knn ))

# Computing the outliers-

modelfit$res <- modelfit$KWH - modelfit$`Predicted value`
deviation <- sd(modelfit[,5])
Final_prediction <- modelfit %>% mutate(Outlier_Tag = ifelse(res >= (2*deviation) , 1, 0))
