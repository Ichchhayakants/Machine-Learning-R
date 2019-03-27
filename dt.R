iris
dim(iris)
s <- sample(150,100)
s
iris_train <- iris[s,]
iris_test <- iris[-s,]
dim(iris_test)


library(rpart)
dtm <- rpart(Species~.,iris_train, method = "class")
dtm
plot(dtm)
text(dtm)
library(rpart.plot)
rpart.plot(dtm)
rpart.plot(dtm, type = 4,extra = 101)

p<- predict(dtm,iris_test[,-5], type="class")

table(iris_test[,5],p)
