install.packages("rjson")
library("rjson")

#Please change the working directory
setwd("E:/neu/Sem 3/Data Science/Midterm/")

# reading the external configuration file for data
json_data <- fromJSON(file="configuration.json")

inputRead <- read.csv("Cleansed.csv")
names(inputRead)


######### k-means #########

## since the data has large number of rows, we decided to take only .001% of total data for better graph plot.
sample_data <- sample(1:nrow(inputRead),round(0.001*nrow(inputRead)))                        
kmeansdata <- inputRead[sample_data,]
names(kmeansdata)
inputRead=scale(kmeansdata[, c(8,9,14,21,22)]) #Scaling the data
inputRead <- kmeansdata[, c(19,20,21)]
names(inputRead)
View(summary(inputRead))

### Reading the configuration from json file. 
if(json_data$Distance_Measure == 'Euclidean'){
  km.out <- kmeans(inputRead,as.numeric(json_data$Clusters),nstart = as.numeric(json_data$nstart))
  
} else if (json_data$Distance_Measure == 'manhattan' || json_data$Distance_Measure == 'correlation'){
  
  km.out <- kmeans(inputRead,as.numeric(json_data$Clusters),iter.max = 1000,nstart=as.numeric(json_data$nstart), method=json_data$Distance_Measure)  
  
} 

km.out$cluster
### plotting the graph for k means on the required k value from json
plot(inputRead, col=(km.out$cluster), main = "K-mean results")


########## Hierarchical clustering ###########

inputRead <- read.csv("Cleansed.csv")
sample_data <- sample(1:nrow(inputRead),round(0.001*nrow(inputRead)))                        
kmeansdata <- inputRead[sample_data,]
#inputRead=scale(kmeansdata[, c(8,9,10,11,12,15,20,21,22,27)]) #Scaling the data
inputRead=scale(kmeansdata[, c(19,20,21)]) #Scaling the data
hc.complete=hclust(dist(inputRead),method="complete") # Complete linkage type
hc.average=hclust(dist(inputRead),method="average")  # Average linkage type

par(mfrow=c(1,2)) #Plotting in a matrix form
plot(hc.complete,main='Complete')
plot(hc.average,main='Average')

# cutting the graph to see the different number of clusters
plot(cutree(hc.complete,as.numeric(json_data$height)))
plot(cutree(hc.average,as.numeric(json_data$height)))



############ Bend Graph ############

sample_data <- sample(1:nrow(inputRead),round(0.001*nrow(inputRead)))                        
kmeansdata <- inputRead[sample_data,]
nrows(kmeansdata)
inputRead=scale(kmeansdata[, c(19,20,21)]) #Scaling the data
wss <- (nrow(inputRead)-1)*sum(apply(inputRead,2,var))
for(i in 2:15){
  wss[i] <- sum(kmeans(inputRead,centers = i)$withinss)
}
plot(2:15,wss,type="l")
plot(1:15, wss,type="b",xlab = "Number of clusters", ylab="Within groups sum of squares")
