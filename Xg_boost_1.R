library(modeldata)
library(tidyverse)
library(caret)
library(xgboost)

data(stackoverflow)

head(stackoverflow)
str(stackoverflow)



dummy.vars<- dummyVars(~ Country, data= stackoverflow, fullRank = TRUE)
df.dummy<- predict(dummy.vars, stackoverflow)

final_data <- cbind(
  stackoverflow%>% select(-Country),  # Keep non-categorical columns
  df.dummy
)

head(final_data)


## Define predictors
y<- as.numeric(stackoverflow$Remote)-1
x<- final_data%>% select(-Remote)

## setting the parameters ####


params<- list(set.seed= 123,
              eval_metric= "auc",
              objective = "binary:logistic")

model<- xgboost(data = as.matrix(x), label = y,
                params = params, nrounds = 20,
                verbose = 1)

## shap_Values
xgb.plot.shap(data = as.matrix(x), model = model, 
              top_n = 5)




# Train-test split
set.seed(123)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
x_train <- x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Train model
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1
)

model <- xgb.train(
  params = params,
  data = xgb.DMatrix(as.matrix(x_train), label = y_train),
  nrounds = 100,
  watchlist = list(train = xgb.DMatrix(as.matrix(x_train), label = y_train),
                   test = xgb.DMatrix(as.matrix(x_test), label = y_test)),
  early_stopping_rounds = 10,
  verbose = 1
)

# Evaluate
pred <- predict(model, as.matrix(x_test))
auc <- roc(y_test, pred)$auc
cat("Test AUC:", auc)










