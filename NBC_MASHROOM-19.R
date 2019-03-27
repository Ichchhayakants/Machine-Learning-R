## Load Data
data <- read.csv(file.choose()) 
str(data)
summary(data)
# Generate a random sample for data partitioning (70:30) 
part <- sample(1:nrow(data),nrow(data)*0.7,replace = F)

part

## Training Data
trainingdata <- data[part,]
##Test Data
testdata <- data[-part,]

nrow(trainingdata)
nrow(testdata)

# Load Library for Naive Bayes Function
library(e1071)

# model building
## formula= class~. -------> column name class against all the columns
model <- naiveBayes(trainingdata$class~., data=trainingdata)
print(model)
str(model)
summary(model)

## Prediction 
pred <- predict(model,testdata)
print(pred)

testdata$pred <- pred

str(testdata)
testdata[,c(2,25)]
## Confusion Matrix
t <- table(testdata$class,pred)

t
## Model accuracy -----> Diagonal elements of t are correctly classified
## Non-diagonal elements are errors
Correct_Classification <- sum(diag(t))/sum(t)*100
Correct_Classification 
Misclassification <- 100- Correct_Classification
Misclassification

## Aliter 
prop <- prop.table(t) ## Prop.table gives  proportion 
MisclassificationError <- sum(diag(prop))*100

