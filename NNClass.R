setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")
data <- read.csv(file.choose())
str(data)

data <- data[,-1]
data$black <-ifelse(data$colour=="Black",1,0)
data$white <- ifelse(data$colour=="White",1,0)
data$male <- ifelse(data$sex=="Male",1,0)
data$female<- ifelse(data$sex=="Female",1,0)
data$empl <- ifelse(data$employed=="Yes",1,0)
data$unempl <- ifelse(data$employed=="No",1,0) 
str(data)

data1 <- data[,-c(2,3,5,6,7)]
str(data1)

newdata<- data1[,-1]
#Min-Max Normalization
normalize<-function(x){
  (x- min(x)) /(max(x)-min(x))
}
dataminmax<-as.data.frame(lapply(newdata, normalize))
head(dataminmax)
dataminmax$released <- data1$released
dataminmax$released <- ifelse(dataminmax$released=="Yes",1,0)
set.seed(222)
sample <- sample(1:nrow(dataminmax), replace=F)
traindata <- dataminmax[sample,]
testdata <- dataminmax[-sample,]

str(dataminmax)
## Neuralnet
library(neuralnet)
n<- neuralnet(released~age+checks+black+white+male+female+empl+unempl,data=traindata,
              hidden = 1, err.fct = "ce", linear.output = F)
plot(n)

out <- compute(n, traindata[,-9])
out 
head(out$net.result)
head(traindata[,9])
head(traindata[1,])


## Confusion matrix
p1 <- out$net.result
pred1 <- ifelse(p1>0.5,1,0)  
tab1 <- table(pred1,traindata$released)  
tab1  
sum(diag(tab1))/sum(tab1)  


p2 <- out$net.result
pred2 <- ifelse(p2>0.5,1,0)  
tab2 <- table(pred2,testdata$released)  
tab2  
sum(diag(tab1))/sum(tab1) 

str(testdata)
