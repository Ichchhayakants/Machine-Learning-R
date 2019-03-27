d <- iris
head(d)
plot(d)

d <- iris[,-5]
plot(d)

cor(d) # by.default pearsons correlation
cor(d,method = "kendall")
cor(d, method = "spearman")
cor.test(d$Sepal.Length,d$Sepal.Width)

cr <- cor(d)
library(corrplot)
corrplot(cr)
# corrplot(cr, method="circle") by default

corrplot(cr, method = "pie")
corrplot(cr,method = "color")
corrplot(cr,method = "number")
corrplot(cr,method = "ellipse")
corrplot(cr,type = "lower")
corrplot(cr,type = "upper")


d <- mtcars
cr <- cor(d)
corrplot(cr)

