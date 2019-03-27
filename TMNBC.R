setwd("C:/Users/Ichchhayakant/Desktop/LAB/ML Lab")
data <- read.csv(file.choose())

dim(data)
names(data)
str(data)
data$v2 <-as.character(data$v2)
data <- data[,c(1,2)]
head(data)

# proportions of ham and spam
table(data$v1)/nrow(data)

## Data preparation
## Remove numbers, punctuations, uninteresting words
## Such as and, but, and or etc


## Text mining package
library(tm)

## Create a corpus (collection of text data)
# One single msg is one single document

smscorpus <- Corpus(VectorSource(data$v2)) ## Corpus creates an r object to store text doc
print(smscorpus)
inspect(smscorpus[1:5])


## Basic text cleaning task
# to lowercase
cleancorpus <- tm_map(smscorpus,tolower)

# Remove numbers
cleancorpus <- tm_map(cleancorpus,removeNumbers)

stopwords()

# Remove stop words
cleancorpus <- tm_map(cleancorpus,removeWords,stopwords())

# Remove punctuations
cleancorpus <- tm_map(cleancorpus,removePunctuation)

# Remove white spaces
cleancorpus <- tm_map(cleancorpus,stripWhitespace)

#cleancorpus <-tm_map(cleancorpus,PlainTextDocument)

inspect(cleancorpus[1:5])


### Tokenization-----> Split the text into individual components
## TOken ---> single element of a text string(Words)

smscorpusdocmatrix <-DocumentTermMatrix(cleancorpus)  ## ---> Take a corpus and create
##a data structure called a spars matrix of N rows and colm= no of words with 0/1 entry

smscorpusdocmatrix

### Split data into train and test
set.seed(1000)
train <-sample(1:nrow(data),nrow(data)*0.7,replace = F) 

trainData <- data[train,]
testData <- data[-train,]
trainingdtm <- smscorpusdocmatrix[train,]
testingdtm <- smscorpusdocmatrix[-train,]
trainingcorpus <- cleancorpus[train]
testingcorpus <- cleancorpus[-train]
table(testData$v1)/nrow(testData)

### Visualizing text data ---> Word Cloud
library(wordcloud)
wordcloud(trainingcorpus,min.freq = 25)
wordcloud(trainingcorpus,min.freq = 25)
wordcloud(trainingcorpus,min.freq = 25)
wordcloud(trainingcorpus,min.freq = 25)

spamdata <- subset(trainData,v1=="spam")
hamdata <- subset(trainData,v1=="ham")
wordcloud(spamdata$v2,min.freq = 10)
wordcloud(hamdata$v2,min.freq = 10)

## Data preparation ---> Creating indicator feature for 
##                       frequent words

findFreqTerms(trainingdtm,5)

convertcount <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels = c(0,1),labels = c("NO","Yes"))
}

smstrain <- DocumentTermMatrix(trainingcorpus,list(dictionary=findFreqTerms(trainingdtm,5)))
smstest <- DocumentTermMatrix(testingcorpus, list(dictionary=findFreqTerms(trainingdtm,5)))
smstrain <- apply(smstrain,MARGIN = 2,convertcount)## Converting matrix into yes no entry

head(smstrain)

smstest <- apply(smstest,MARGIN = 2,convertcount)
head(smstest)


library(e1071)
model <- naiveBayes(smstrain,trainData$v1,laplace = 1)
model
p <- predict(model,smstest,type = "class")
p
table(p,testData$v1)
 