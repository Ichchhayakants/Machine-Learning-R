setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")
data <- read.csv(file.choose())
str(data)
plot(data)
set.seed(1000)
sample <- sample(1:nrow(data),nrow(data)*0.7,replace = F)
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

normalize<-function(x){
  (x- min(x)) /(max(x)-min(x))
}
dataminmax<-as.data.frame(lapply(data1, normalize))
head(dataminmax)
traindata <- dataminmax[sample,]
testdata <- dataminmax[-sample,]


allVars<- colnames(data1)
predictorVars <- allVars[!allVars%in%"charges"]
predictorVars <- paste(predictorVars,collapse = "+")

form= as.formula(paste("charges~",predictorVars,collapse = "+"))

library(neuralnet)
neuralModel <- neuralnet(formula=form, hidden=c(4,2),linear.output = T, 
                          data = traindata)
neuralModel
## Result options
names(neuralModel)

plot(neuralModel)

neuralModel$result.matrix

out <- cbind(neuralModel$covariate,neuralModel$net.result[[1]])
head(out)
str(data1)
dim(out)
dimnames(out) <- list(NULL, 
                      c("age", "bmi","children","smoker","charges","male","female",
                        "northeast","northwest","southeast"))

head(neuralModel$generalized.weights[[1]])


str(testdata)

predictions <- compute(neuralModel,testdata[,-5])
str(predictions)
predictions$net.result

## predicting and unscalling
predictions <- predictions$net.result*(max(testdata$charges)-min(testdata$charges))+min(testdata$charges)

actualValues <- (testdata$charges)*(max(testdata$charges)-min(testdata$charges))+ min(testdata$charges)


MSE <- sum((predictions-actualValues)^2)/nrow(testdata)
MSE


plot(testdata$charges, predictions, col='blue', main = 'Real vs Predicted',pch=1, cex=0.9, type="p", xlab = "Actual", ylab= "Predicted")



