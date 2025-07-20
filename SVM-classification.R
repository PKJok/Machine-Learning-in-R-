
data(iris)
head(iris)

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(tibble)
library(doSNOW)
# plot to extract feature
plot(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col= iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col= iris$Species)

sum(is.na(iris))
str(iris)

# split data using caret
set.seed(42)
 train_index<- createDataPartition(iris$Species, p=0.7, list = FALSE)
train_iris<- iris[train_index,] 
test_iris<- iris[-train_index,]

# look at the proportions
prop.table(table(iris$Species))
prop.table(table(train_iris$Species))
prop.table(table(test_iris$Species))
str(train_iris)


##########################################  Normal SVM ###########################################
svm_fit<- svm(Species~., data = train_iris, kernel = "linear", cost = .1, scale = FALSE)
print(svm_fit)

plot(svm_fit, train_iris, Petal.Width ~ Petal.Length)


################################################ TUNED SVM ################################
# using caret

# parallel setup # always start from parallel setup

cl<- makeCluster(5, type= "SOCK")
registerDoSNOW(cl)

# cross validation control (5-fold, repeated 3 times)
train.control<- trainControl(method = "repeatedcv", number = 5, 
                             repeats = 3, search = "grid")

#_____________ tuning of radial SVM requires "sigma"  _________
tune.grid.radial<- expand.grid(sigma = c(0 ,0.001, 0.01, 0.1,  1),
                        C =c(0.001, 0.01, 0.1, 1 ,10, 100))

# ___________________ tuning of linear svm only requires "Cost = C" _________________
tune.grid.linear<- expand.grid(C= c(0.01,0.1,1,10))

#svm_tune_radial<- train(Species ~ ., data = train_iris, method = "svmRadial", 
                        #tuneGrid = tune.grid, trControl= train.control)

#stopCluster(cl)

#svm_tune_radial

# svm linear

svm_tune_linear<- train(Species ~ ., data = train_iris, method = "svmLinear", 
                 tuneGrid = tune.grid.linear, trControl= train.control)
stopCluster(cl)

svm_tune_linear # cost = 1, shows the best result

plot(svm_tune_linear)

pred<- predict(svm_tune_linear, test_iris)

confusionMatrix(pred, test_iris$Species)


################### SVM using (cost = 1) ##############################

# Create a grid over two predictors
xrange <- seq(min(train_iris$Petal.Length), max(train_iris$Petal.Length), length.out = 200)
yrange <- seq(min(train_iris$Petal.Width), max(train_iris$Petal.Width), length.out = 200)

grid <- expand.grid(Petal.Length = xrange, Petal.Width = yrange)

# Need to add dummy columns for the unused predictors
grid$Sepal.Length <- mean(train_iris$Sepal.Length)
grid$Sepal.Width  <- mean(train_iris$Sepal.Width)

# Reorder to match training data column order
grid <- grid[, names(train_iris)[names(train_iris) != "Species"]]

# Predict using final model
grid$Species <- predict(svm_tune_linear, newdata = grid)

# Plot
library(ggplot2)
library(RColorBrewer)

ggplot(train_iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size = 2) +
  geom_tile(data = grid, aes(fill = Species), alpha = 0.2, color = NA) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "SVM Decision Boundary (caret model)") +
  theme_minimal()







