setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")
data <- read.csv(file.choose())
str(data)
plot(data)
set.seed(1000)
sample <- sample(1:nrow(data),nrow(data)*0.7,replace = F)
traindata <- data[sample,]
testdata <- data[-sample,]

##Regression model
model_LR <-lm(charges~., data=traindata)
model_LR
summary(model_LR)

p_LR <- predict(model_LR,testdata)
cor(p_LR,testdata$charges)^2


## Regression tree
library(rpart)
model_RT <- rpart(charges~., data=traindata)
model_RT
library(rpart.plot)
rpart.plot(model_RT)
p_RT <- predict(model_RT,testdata)
cor(p_RT,testdata$charges)^2


##Modeltree 
library(RWeka)
model_MT <-M5P(charges~.,traindata)
model_MT
p_MT <- predict(model_MT, testdata)
cor(p_MT,testdata$charges)^2


## Neuralnet
library(neuralnet)
head(data)
## Normalize the data
str(data)
data1 <- data
data1$male <- ifelse(data1$sex=="male",1,0)
data1$female <- ifelse(data1$sex=="female",1,0)
data1$smoker <-ifelse(data1$smoker=="yes",1,0)
data1$northeast <- ifelse(data1$region=="northeast",1,0)
data1$northwest <- ifelse(data1$region=="northwest",1,0)
data1$southeast <- ifelse(data1$region=="southeast",1,0)
str(data1)
data1 <- data1[,-c(2,6)]
str(data1)

data1_z <-scale(data1)
head(data1_z)
str(data1_z)
traindata <- data1_z[sample,]
testdata <- data1_z[-sample,]

## Normalizing
normalize<-function(x){
  (x- min(x)) /(max(x)-min(x))
}
dataminmax<-as.data.frame(lapply(newdata, normalize))



model_NN <- neuralnet(charges~age+bmi+children+smoker+male+female+northeast+nortwest+southeast,traindata,hidden = 1)
model_NN
plot(model_NN)
p_NN <- compute(model_NN, testdata[,-5])
p_NN
cor(p_NN$net.result,testdata[,5])^2


model_NN <- neuralnet(charges~age+bmi+children+smoker+male+female+northeast+nortwest+southeast,traindata,hidden = 3)
p_NN <- compute(model_NN, testdata[,-5])
cor(p_NN$net.result,testdata[,5])^2


