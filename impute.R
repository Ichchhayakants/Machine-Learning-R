library(mice)
library(VIM)


data <- read.csv(file.choose(),header = T)
data1 <- read.csv(file.choose(),header = T)
str(data)
summary(data)

## Missing data proportion
p <- function(x){
  sum(is.na(x))/length(x)*100
}
apply(data,2,p) # 2 for column ,1 for row
apply(data1,2,p)

# pattern of missing data
md.pattern(data)
md.pattern(data1)

md.pairs(data)


marginplot(data[,c("Age","Pclass")])

marginplot(data[,c('Age','Name')])


# Impute
impute <- mice(data[,c(1,6)],m=3, seed = 123)
print(impute)
impute$imp$Age
data[889,]
summary(data$Age)

newdata <- complete(impute,1) # 1st impute out of 3
