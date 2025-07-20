# SVM practice 

data(iris)
iris

library(caret)
# create the indicator variable
head(iris)

# check missing data

iris<- as.data.frame(iris)
sum(is.na(iris$Species)) # 50 NA in Species in the data frame 
# remove the NA data
iris<- na.omit(iris)
# check 
sum(is.na(iris$Species))

###################### Modify Data ##################################
str(iris)

iris$Species<- as.character(iris$Species)

iris[iris$Species == "setosa",]$Species<-"0"
iris[iris$Species == "versicolor",]$Species<-"1"
iris[iris$Species == "virginica",]$Species<-"2"

str(iris)
unique(iris$Species)

# change the str of "Species" to numeric
iris$Species<- as.numeric(iris$Species)
str(iris)

#################### Training and Testing data ######################
set.seed(420)

train_index<- createDataPartition(iris$Species, p=0.7, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

dim(train_data)
dim(test_data)

train_data$Species<- as.factor(train_data$Species)

######### model of SVM #################

trctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_linear<- train(Species~., data = train_data, method = "svmLinear",
            trControl= trctrl,
            preProcess= c("center","scale"),
            tuneLength= 10)
svm_linear

predict_linear<- predict(svm_linear, newdata = test_data)

predict_linear

confusionMatrix(table(predict_linear, test_data$Species)) # nice confusion matrix



################################# compare SVm Linear vs SVM Radial ##############################

svm_radial<- train(Species~., data = train_data, method= "svmRadial",
                   preProcess= c("center","scale"),
                   tuneLength= 10)
svm_radial

predict_radial<- predict(svm_radial, newdata = test_data)


confusionMatrix(table(predict_radial, test_data$Species))




