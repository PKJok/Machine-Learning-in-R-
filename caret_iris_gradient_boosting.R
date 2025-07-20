library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(randomForest)
library(pROC)

data(iris)

head(iris)

str(iris)

featurePlot(x= iris[,1:4], y= iris$Species, plot = "pairs")
featurePlot(x= iris[,1:4], y= iris$Species, plot = "ellipse")

featurePlot(x= iris[,1:4], y= iris$Species, plot = "ellipse", auto.key= list(columns=3))
featurePlot(x= iris[,1:4], y= iris$Species, plot = "ellipse", auto.key=list())

featurePlot(x= iris[,1:4], y= iris$Species, plot = c("boxplot"), auto.key= list(columns=3), 
            scales=list(y=list(relation = "free")),
            labels = c("species","length in cm"))
featurePlot(x= iris[,1:4], y= iris$Species, plot = "density", auto.key= list(columns=3), labels = c("Species",""),
            scale= list(x = list(relation = "free"), y= list(relation = "free")), pch = "|")

cor_iris<- cor(iris[,1:4])

corrplot(cor_iris, method = "ellipse", type = "lower")



#outcome variable ; numeric # use Regression models

# outcome variable: categorical # use classification model

# we are trying to predict sepal length (numeric) # use regression model

# cross validation = by using caret package 
str(iris)

train.control<- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model_lm<- train(Sepal.Length~., iris, method = "lm", trControl= train.control)

summary(model_lm)
model_lm

plot(model_lm$finalModel)


## test the model_lm

obs= predict(model_lm, iris)

plot(iris$Sepal.Length, obs)
abline(0,1)


plot(1: nrow(iris), iris$Sepal.Length)
points(1:nrow(iris), obs, col="red")

######### random forest #######
#### categorical variable for classification ############

model_rf<- train(Species~., iris, method = "rf", trControl= train.control)
model_rf
summary(model_rf)

obs= predict(model_rf, iris)

confusionMatrix(iris$Species, obs)

##################### gradient boost model #################
model_gbm<- train(Species~., iris, method = "gbm", trControl= train.control)
model_gbm

obs= predict(model_gbm, iris)

confusionMatrix(iris$Species, obs)






df<- iris

unique(df$Species)
df<- df%>%
  mutate(Species = as.numeric(Species, levels = c("setosa","versicolor","virginica"),
                          labels = c(0,1,2)))
unique(df$Species)  

model_lm<- train(Species~., data = df, method= "lm",trControl= train.control)
summary(model_lm)











