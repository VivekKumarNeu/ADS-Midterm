install.packages("forecast")
install.packages("devtools")
install.packages("broom")
install.packages("ROCR")
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)
library(ROCR)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################################################
##Preprocessing
inputRead <- read.csv("Cleansed.csv")
#######################################
##For all buildings
table(inputRead$Base_Hour_Class)/nrow(inputRead)
#High       Low 
#0.5379025 0.4620975 
smp_size=floor(0.54*nrow(inputRead))
set.seed(123)
train_ind<-sample(seq_len(nrow(inputRead)),size=smp_size)
train<-inputRead[train_ind,]
test<-inputRead[-train_ind,]
fit<-glm(Base_Hour_Class~BuildingID+Consumption_Type+Meter_Number+Month+Hour+TemperatureF+Dew_PointF+Humidity+Sea_Level_PressureIn+WindDirDegrees+KWH+DayOfWeek+WeekDay,data=train,family=binomial(link="logit"))
summary(fit)
test$predictions<-predict(fit,test,type='response')
pred<-rep("Low",length(test$predictions))
pred[test$predictions>=0.5]<-"High"
write.csv(test,"LogisticRegression_Prediction.csv",row.names=FALSE)
cm<-confusionMatrix(test$Base_Hour_Class,pred)
str(cm)
tocsv <- data.frame(cbind(t(cm$overall),t(cm$byClass)))
write.csv(tocsv,file="LogisticRegression_ConfusionMatrix.csv",row.names=FALSE)

prediction<-prediction(test$predictions,test$Base_Hour_Class)
performance<-performance(prediction,measure="tpr",x.measure="fpr")
plot(performance,main="ROC Curve",xlab="1-Specificity",ylab="Sensitivity")
########################################

# compute<-function(dframe){
#   tryCatch({
#     smp_size=floor(0.75*nrow(dframe))
#     set.seed(123)
#     train_ind<-sample(seq_len(nrow(dframe)),size=smp_size)
#     train<-dframe[train_ind,]
#     test<-dframe[-train_ind,]
#     fit<-glm(Base_Hour_Class~Month+Hour+TemperatureF+Dew_PointF+Humidity+Sea_Level_PressureIn+WindDirDegrees,data=dframe,family=binomial(link="logit"))
#     summary(fit)
#     test$predictions<-predict(fit,test,type='response')
#     pred<-rep("Low",length(test$predictions))
#     pred[test$predictions>=0.5]<-"High"
#     confusionMatrix(test$Base_Hour_Class,pred)
#   },error = function(err) {
#     
#     # error handler picks up where error was generated
#     print(paste("MY_ERROR:  ",err))
#     
#   })
# }
# 
# 
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# dataRead <- read.csv("Cleansed.csv",stringsAsFactors = FALSE)
# 
# inputDateRead_Group <- split(dataRead, with(dataRead, interaction(BuildingID,Meter_Number)), drop = TRUE) 
# 
# for(i in 1:NROW(inputDateRead_Group))
# {
#   compute(inputDateRead_Group[[i]])
# }



############################################################################################################
# KNN Classification
#########################
#install.packages("forecast")
#install.packages("devtools")
#install.packages("broom")
#install.packages("ROCR")
install.packages("gmodels")

library(gmodels)
library(class)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dataRead <- read.csv("Cleansed.csv",stringsAsFactors = FALSE)
inputDateRead_Group <- split(dataRead, with(dataRead, interaction(BuildingID,Meter_Number)), drop = TRUE)

inputRead<-inputDateRead_Group[[1]]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

inputReadNumeric <- subset(inputRead, select=c('TemperatureF','Dew_PointF','Humidity','Sea_Level_PressureIn','VisibilityMPH','WindDirDegrees','KWH','WeekDay','DayOfWeek','Base_Hour_Class'))
inputReadNormalized <- as.data.frame(lapply(inputReadNumeric[,c('TemperatureF','Dew_PointF','Humidity','Sea_Level_PressureIn','VisibilityMPH','WindDirDegrees','KWH','WeekDay','DayOfWeek')], normalize))
summary(inputReadNormalized)
  smp_size=floor(0.25*nrow(inputReadNormalized))
set.seed(123)
train_ind<-sample(seq_len(nrow(inputReadNormalized)),size=smp_size)
train<-inputReadNormalized[train_ind,]
test<-inputReadNormalized[-train_ind,]
train_target<-inputReadNumeric[train_ind,10]
test_target<-inputReadNumeric[-train_ind,10]
#install.packages("class")
library(class)
m1<-knn(train=train,test=test,cl=train_target,k=80)
knn<-confusionMatrix(test_target,m1)
tocsv <- data.frame(cbind(t(knn$overall),t(knn$byClass)))
write.csv(tocsv,file="KNN_ConfusionMatrix.csv",row.names=FALSE)

summary(m1)
test$predictions<-m1
write.csv(test,"KNN_Prediction.csv",row.names=FALSE)
#Evaluate the model performance
#install.packages("gmodels")

library(gmodels)

#confusion matrix
cs<-CrossTable(x=test_target, y=m1, prop.chisq = FALSE)

####################################################################################################
# Random Forest
#############################################

#install.packages("randomForest")
library(randomForest)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dataRead <- read.csv("Cleansed.csv",stringsAsFactors = FALSE)
inputDateRead_Group <- split(dataRead, with(dataRead, interaction(BuildingID,Meter_Number)), drop = TRUE)
inputRead<-inputDateRead_Group[[1]]
set.seed(123)
library(ggplot2)

inputRead$Base_Hour_Class <- as.factor(inputRead$Base_Hour_Class)
inputRead <- inputRead[,c('TemperatureF','Dew_PointF','Humidity','Sea_Level_PressureIn','VisibilityMPH','WindDirDegrees','KWH','WeekDay','DayOfWeek','Base_Hour_Class')]
library(randomForest)
table(inputRead$Base_Hour_Class)/nrow(inputRead)
#High       Low 
#0.4680753 0.5319247 

smp_size=floor(0.46*nrow(inputRead))
set.seed(123)
train_ind<-sample(seq_len(nrow(inputRead)),size=smp_size)

train <- inputRead[train_ind,]
test <- inputRead[-train_ind,]

colNames <- names(train)

colNames <- colNames[!colNames %in% c("Base_Hour_Class")]


concatCols <- paste(colNames, collapse = "+")


concatVal <- as.formula(paste("Base_Hour_Class", concatCols, sep = " ~ "))



randomForest <- randomForest(concatVal,
                             train,
                             ntree=100,
                             importance=T)
plot(randomForest)


varImpPlot(randomForest,
           sort = T,
           main="Variable Importance",
           n.var=5)

#install.packages("caret")
library(e1071)
library(caret)



test$predictions <- predict(randomForest ,test)
write.csv(test,"RandomForest_Predictions.csv",row.names=FALSE)

rf<-confusionMatrix(data=test$predictions,
                    reference=test$Base_Hour_Class,
                    positive='Low')
tocsv <- data.frame(cbind(t(rf$overall),t(rf$byClass)))
write.csv(tocsv,file="RF_ConfusionMatrix.csv",row.names=FALSE)
