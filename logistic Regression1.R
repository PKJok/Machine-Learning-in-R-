data(iris)

library(ggplot2)
library(cowplot)
## NOTE: The data used in this demo comes from the UCI machine learning
## repository.
## http://archive.ics.uci.edu/ml/index.php
## Specifically, this is the heart disease data set.
## http://archive.ics.uci.edu/ml/datasets/Heart+Disease

url <- "https://raw.githubusercontent.com/StatQuest/logistic_regression_demo/master/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

#####################################
##
## Reformat the data so that it is
## 1) Easy to use (add nice column names)
## 2) Interpreted correctly by glm()..
##
#####################################
head(data) # you see data, but no column names

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

str(data)

data$cp<- as.factor(data$cp)
data[data == "?"]<- NA
data[data$sex == 0,]$sex<-"F"
data[data$sex == 1,]$sex<-"M"
data$sex<- as.factor(data$sex)
data$fbs<- as.factor(data$fbs)
data$restecg<- as.factor(data$restecg)
data$exang<- as.factor(data$exang)
data$slope<- as.factor(data$slope)

data$ca<- as.integer(data$ca)
data$ca<- as.factor(data$ca)
data$thal<- as.integer(data$thal)
data$thal<- as.factor(data$thal)
str(data)
data$hd <- ifelse(test = data$hd == 0, yes = "Healthy", no = "Unhealthy")
data$hd<- as.factor(data$hd)
str(data)

# chek the na values

nrow(data[is.na(data$ca)| is.na(data$thal),])
data[is.na(data$ca)| is.na(data$thal),]

# remove the NA value

nrow(data)
data<- data[!(is.na(data$ca)| is.na(data$thal)),]
nrow(data)

# check the heart disease in male and female 
xtabs(~hd+sex, data = data)

# looks like the data is represented well in both sex

xtabs(~hd+cp, data = data)

xtabs(~hd+fbs, data = data)

xtabs(~hd+restecg, data = data) # looks like we have some problem but left it now

# now perform generalized linear model , we predict the  heart disease only using gender of each patient

logistic<- glm(hd~sex, data = data, family = "binomial")
summary(logistic)

data1<- data
data1$hd<- as.character(data1$hd)
  
plot(data1$hd, data1$sex)



logistic<- glm(hd~., data=data, family = "binomial")
summary(logistic)


















