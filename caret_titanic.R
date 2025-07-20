
setwd("C:/Users/ASUS/OneDrive/Desktop/Machine Learning")

library(readr)
library(caret)
library(doSNOW)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# caret
# 1. Huge ecosystem creating new packages
# 2. we can find new packages in R also neural network
# 3. different pakage = different coding type = can create problem working with these packages of ML
# caret helps:______________________________________________________________________________________________________
# 1. @ Provide common interface for 200 ML algorithms 
# 2. like when you want to do "logistic regression" and "svm". By using caret we can copy and change only 1-2 parameter, 
#    making easier for performing ML algorithms
# 3. helps in Data splitting and sampling (helps to create stratified random samples, down samples, up samples, )
# 4. if we want to synthetically create data for your class by using smoke : caret got it baby!!!
# 5. Feature Selection 
# 6. Model tuning : great model has many parameters, so, makes it difficult to work with 
# 7. 


df <- read.csv("Titanic-Dataset.csv", row.names = 1)
head(df)

# explore the data

df$Cabin
str(df)

table(df$Embarked)

sum(is.na(df$Embarked))
sum(df$Embarked == "") # 2 missing value in character

# Replace the missing embarked values with mode
df$Embarked[df$Embarked == ""]<- "S"
table(df$Embarked)


# adding feature for tracking missing ages

summary(df$Age) #177 missing value
df$MissingAge<- ifelse(is.na(df$Age), 
                          "Y","N")

############################ XGBOOST #######################################

# is a algorithm make collection of decision trees, working together to make ,more powerful model
# decision trees - tends to prefer fewer more powerful features


df$FamilySize<- 1 + df$SibSp + df$Parch

# setup factors
df$Survived<- as.factor(df$Survived)
df$Pclass<- as.factor(df$Pclass)
df$Sex<- as.factor(df$Sex)
df$Embarked<- as.factor(df$Embarked)
df$MissingAge<- as.factor(df$MissingAge)

head(df)

# Subset data to features we wish ko keep
# lets remove less imp features

df<- df%>%
  select(-3,-8,-10) # remove { name, cabin, ticket}
colnames(df)
# the features i kept are:
Features<- c("Survived", "Pclass","Sex", "Age","SibSp","Parch","Fare" ,"Embarked","MissingAge", "FamilySize")

str(df)

################## Caret : automatically impute missing Age ##################

# Caret Uses for Imputation:
# 1. Median
# 2. KNN 
# 3. Bag decision tree (#bagImpute) (most accurate, $$$$ computationally intensive $$$, but our data set is smaller "OK")

#______________________ Imputation in Caret only works with numeric data, doesn't works with factors ____________________________#
# ~~~~~~~~~~~~~~ we can use dummy variables to transform all features 
# we want to not do this to the main factor " Survived"

dummy.vars<- dummyVars(~., data = df[,-1])
df.dummy<- predict(dummy.vars, df[,-1]) # changed into binary form for factors which is numeric
view(df.dummy)

# Begin the impute

############################## Pre-Process #############################
# create imputation for every columns 
# computationally intensive


pre.process<- preProcess(df.dummy, method= "bagImpute") # bag Impute = bagging form random forest
imputed.data<- predict(pre.process, df.dummy)
view(imputed.data) # missing Age has been filled # fills the NA value in Age

# put the filled Age to the original data
sum(is.na(df$Age))

df$Age<- imputed.data[,6]
sum(is.na(df$Age))

head(df)
############# Split the data ###############

set.seed(42)
train_index<- createDataPartition(df$Survived, p=0.7, list = FALSE, times = 1)
train_df<- df[train_index,]
test_df<- df[-train_index,]

head(train_df)
# examine the proportions of survived class label across
prop.table(table(df$Survived))
prop.table(table(train_df$Survived))   # shows the same ratios
prop.table(table(test_df$Survived)) 


############ train model ##############
# set up caret to perform 10 fold cross validation repeated $$ 3 times $$
# 10 fold cross validation : split data into 10 ways, cycle through all the folds a total of 10 times, 
#                                                                                                     build 10 models and evaluate them to the data
# times and to use a "grid search" => gives the factors to control for tuning, for optimal "hyper parameter tuning"
#train control = > this is same for every ML models, neural network <=

train.control<- trainControl(method = "repeatedcv", number = 10, 
                             repeats = 3, search = "grid")

# leverage a grid search of hyperparameters for Xgboost
 #tune.grid<- expand.grid(eta = c(0.5, 0.075, 0.1),
                         #nrounds = c(50, 75, 100),
                         #max_depth = 6:8,
                         #min_child_weight = c(2.0, 2.25, 2.5),
                         #colsample_bytree = c(0.3, 0.4, 0.5),
                         #gamma= 0,
                         #subsample= 1)



#view(tune.grid)



tune.grid1<- expand.grid(eta = 0.1,
                        nrounds = 75,
                        max_depth = 8,
                        min_child_weight = 2.0,
                        colsample_bytree =  0.5,
                        gamma= 0,
                        subsample= 1)
                        

# use doSNOW package to enable caret to train in parallel
# while there are many package options 
# 
# create a socket cluster using 10 processes
# NOTE: Tune this number based on the number of cores/threads 
# available in my machine

cl<- makeCluster(5, type= "SOCK")

# register the cluster so that caret will know to train in parallel
registerDoSNOW(cl)

# train XG boost model using 10 fold cv repeated 3 times
# and hyperparameter grid search to train in the optimal model

caret.cv<- train(Survived ~ ., 
                 data = train_df, 
                 method= "xgbTree",
                 tuneGrid = tune.grid1,
                 trControl= train.control)

stopCluster(cl)

# result
caret.cv 

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were nrounds = 75, max_depth = 8, eta = 0.1, gamma = 0, 
# colsample_bytree = 0.5, min_child_weight = 2 and subsample = 1.

plot(caret.cv$results$Accuracy, col= "#FE6789")

accuracy<- as.data.frame(caret.cv$results)
accuracy%>%
  arrange(-Accuracy)%>%
  head()

# predict on the test_df
pred<- predict(caret.cv, test_df)

# confusion matrix

confusionMatrix(pred, test_df$Survived)


rf_model <- train(
  Survived ~ .,
  data = train_df,
  method = "rf",
  trControl = train.control,
  tuneGrid = data.frame(mtry = 2),
  ntree = 200
)

rf_model

pred1<- predict(rf_model, test_df)
confusionMatrix(pred1, test_df$Survived)



















































