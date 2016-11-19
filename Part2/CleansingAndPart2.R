#install.packages("dplyr")
library(dplyr)
library(plyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
consolidate <- read.csv("Final.csv", header=TRUE)
baseHourFrame<-filter(consolidate, BaseHourFlag == "TRUE")
agg<-aggregate(KWH ~ BuildingID+Consumption_Type+Meter_Number+WeekDay+Month+Holiday,data=baseHourFrame,mean)
agg<-filter(agg, Holiday == "FALSE")
agg<-filter(agg,WeekDay=="1")
agg <- subset(agg, select = -c(Holiday,WeekDay) )
final<-merge(x = consolidate, y = agg, by = c("BuildingID","Consumption_Type","Meter_Number","Month"), all.x = TRUE)
names(final)[names(final)=="KWH.x"] <- "KWH"
names(final)[names(final)=="KWH.y"] <- "Base_Hour_Usage"
final$KWH<-as.numeric(final$KWH)
final$Base_Hour_Usage<-as.numeric(final$Base_Hour_Usage)
final$Base_Hour_Class<-ifelse(final$KWH>final$Base_Hour_Usage, "High", "Low")
write.csv(final, "temp.csv",row.names=FALSE)

#######################
Cleansing
#######################
#install.packages("zoo")

#####
#Removing na and Outliers
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
inputRead <- read.csv("temp.csv")
View(summary(inputRead))

library(zoo)
inputRead$TemperatureF[is.na(inputRead$TemperatureF)] <-na.approx(inputRead$TemperatureF)
#na.approx(inputRead$TemperatureF)
View(summary(inputRead))

boxplot(inputRead$TemperatureF)

inputRead[inputRead$TemperatureF < -1000,]

out <- which(inputRead$TemperatureF < -1000, arr.ind=TRUE)

inputRead <- inputRead[-out,]
boxplot(inputRead$TemperatureF)
#########################################
inputRead$Dew_PointF[is.na(inputRead$Dew_PointF)] <-na.approx(inputRead$Dew_PointF)
#na.approx(inputRead$TemperatureF)
View(summary(inputRead))
boxplot(inputRead$Dew_PointF)
out <- which(inputRead$Dew_PointF < -10, arr.ind=TRUE)
inputRead <- inputRead[-out,]
boxplot(inputRead$Dew_PointF)
#############################################
inputRead$Humidity[is.na(inputRead$Humidity)] <-na.approx(inputRead$Humidity)
View(summary(inputRead))
boxplot(inputRead$Humidity)
#inputRead[inputRead$Humidity < 30,]
out <- which(inputRead$Humidity < 35, arr.ind=TRUE)
inputRead <- inputRead[-out,]
boxplot(inputRead$Humidity)
#############################################
inputRead$Sea_Level_PressureIn[is.na(inputRead$Sea_Level_PressureIn)] <-na.approx(inputRead$Sea_Level_PressureIn)
View(summary(inputRead))
boxplot(inputRead$Sea_Level_PressureIn)
#inputRead[inputRead$Humidity < 30,]
#######################################
inputRead$WindDirDegrees[is.na(inputRead$WindDirDegrees)] <-na.approx(inputRead$WindDirDegrees)
View(summary(inputRead))
boxplot(inputRead$WindDirDegrees)
############################
inputRead$Gust_SpeedMPH[is.na(inputRead$Gust_SpeedMPH)] <-na.approx(inputRead$Gust_SpeedMPH)
View(summary(inputRead))
boxplot(inputRead$Gust_SpeedMPH)
out <- which(inputRead$Gust_SpeedMPH > 5, arr.ind=TRUE)
inputRead <- inputRead[-out,]
boxplot(inputRead$Gust_SpeedMPH)
##############
inputRead$VisibilityMPH[is.na(inputRead$VisibilityMPH)] <-na.approx(inputRead$VisibilityMPH)
View(summary(inputRead))
boxplot(inputRead$VisibilityMPH)
out <- which(inputRead$VisibilityMPH > 15, arr.ind=TRUE)
inputRead <- inputRead[-out,]
boxplot(inputRead$VisibilityMPH)
#####################
inputRead$PrecipitationIn<-NULL
write.csv(inputRead[,], file = "Cleansed.csv",row.names=FALSE)
####################################################################################################