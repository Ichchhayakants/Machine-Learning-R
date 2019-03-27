
setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")
data<- read.csv("train.csv", header = T)
 data
train <- sample(1:nrow(data),nrow(data)*.7, replace = F) 

data$Survived <- as.factor(data$Survived)
traindata <- data[train,]
testdata <- data[-train,]



library(e1071)
model <- naiveBayes(Survived~., data=traindata)
model

pred <- predict(model,testdata,type="raw")

testdata$pred <- ifelse(pred[,1]>pred[,2],0,1)
table(testdata$Survived,testdata$pred)


testdata$predict <- ifelse(testdata$Survived==testdata$pred,1,0)
(sum(testdata$predict)/nrow(testdata)*100)
