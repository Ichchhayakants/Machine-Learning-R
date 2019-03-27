cardiographic  <- read.csv(file.choose())
str(cardiographic)
data <- cardiographic
str(data)
data$NSPF <- factor(data$NSP)
set.seed(1234)
pd <- sample(2,nrow(data), replace = T, prob=c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]
library(party)
tree <- ctree(NSPF~BPM+APC+FMPS,data=train)
tree
plot(tree)


## Pruning The tree
tree2 <- ctree(NSPF~BPM+APC+FMPS,data=train, controls = ctree_control(mincriterion = 0.99,minsplit = 500))
tree2
plot(tree2)


## Predict 
predict(tree, validate, type="prob")
predict(tree, validate)


library(rpart)
tree3 <- rpart(NSPF~BPM+APC+FMPS,data=train)
plot(tree3)
text(tree3)
library(rpart.plot)
rpart.plot(tree3, extra = 101,type=4)

predict(tree3,validate)



## Misclassification error for train data
tab <- table(predict(tree), train$NSPF)
print(tab)
1- sum(diag(tab))/sum(tab)


##Misclassification error for test data
testPred <- predict(tree,newdata = validate)
tab <- table(testPred, validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

library(pROC)
sum(diag(tab))/sum(tab)
predi <- predict(tree3,validate,type = "prob")
auc <- auc(validate$NSPF,predi[,1])
plot(roc(validate$NSPF,predi[,1]))

View(data)
