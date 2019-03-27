library(MASS)
library(rpart)
library(rpart.plot)

set.seed(123)
DataFrame <- birthwt
help("birthwt")
str(DataFrame)
dim(DataFrame)


## Check the unique values 
apply(DataFrame,2,function(x) round(length(unique(x))/nrow(DataFrame),3)*100)

cols <- c("low","race","smoke","ptl","ht","ui","ftv")


for (i in cols) {
  DataFrame[,i]=as.factor(DataFrame[,i])
  
}
str(DataFrame)


library(caTools)

ind <- sample.split(Y=DataFrame$low,SplitRatio = 0.8)
trainDF <-  DataFrame[ind,]
testDF <- DataFrame[-ind,]
DecisionTreeModel <- rpart(low~.-bwt,data=trainDF,method = "class")
plot(DecisionTreeModel)
text(DecisionTreeModel,pretty = 0)
summary(DecisionTreeModel)


predi <- predict(DecisionTreeModel,testDF,type = "class")
t <- table(predi,testDF$low)
t

library(pROC)
sum(diag(t))/sum(t)
predi <- predict(DecisionTreeModel,testDF,type = "prob")
auc <- auc(testDF$low,predi[,2])
plot(roc(testDF$low,predi[,2]))
