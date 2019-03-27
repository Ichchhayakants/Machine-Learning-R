setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")

rules <-read.csv(file.choose())
url_file <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
rules <- read.csv(url(url_file), header=FALSE)
fields <- c("class",
            "cap_shape",
            "cap_surface",
            "cap_color",
            "bruises",
            "odor",
            "gill_attachment",
            "gill_spacing",
            "gill_size",
            "gill_color",
            "stalk_shape",
            "stalk_root",
            "stalk_surface_above_ring",
            "stalk_surface_below_ring",
            "stalk_color_above_ring",
            "stalk_color_below_ring",
            "veil_type",
            "veil_color",
            "ring_number",
            "ring_type",
            "spore_print_color",
            "population",
            "habitat")
colnames(rules) <- fields
head(rules)

write.csv(file = "mush.csv",rules)
dim(rules)
str(rules)
rules <- rules[,-17]
str(rules)      
table(rules$class)/nrow(rules)


set.seed(1000)
train <- sample(1:nrow(rules), nrow(rules)*0.7,replace=F)
traindata <- rules[train,]
testdata<- rules[-train,]

library(RWeka)
model <- OneR(class~.,data=traindata)
model
pred <- predict(model,testdata)
pred
table(pred,testdata$class)
summary(model)
model1<- JRip(class~.,data=traindata)
model1
pred1 <- predict(model1,testdata)
pred1
table(pred1,testdata$class)
