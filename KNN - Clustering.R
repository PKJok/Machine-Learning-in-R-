## First Load Necessary Library ###
library(factoextra) ## Visualize the Results of Multivariate Data Analysis
library(ggfortify)  ## Data visualization Tools for Statistical Analysis Results 

data(iris)
head(iris)

measure_dist<- dist(iris[,-5], method = "euclidean")
head(measure_dist)

iris_features<- iris[,1:4]
#scale the features
iris_scaled<- scale(iris_features)


## plot Elbow Plot ###
fviz_nbclust(iris_scaled, kmeans, method = "wss")+ 
  # within-cluster sum of squares     ## found 3 is the optimal ##
  geom_vline(xintercept = 3, linetype =2) #so, drawing at intercept 3


## applying k means clustering ###
set.seed(420)
kmeans_result<- kmeans(iris_scaled, centers = 3, nstart =25) ## Runs K-means 25 times with random initial centroids

## visualize the cluster 

fviz_cluster(kmeans_result, data = iris_scaled, 
             palette = "jco", 
             ggtheme = theme_minimal())


### Viewing in ggfortify ###
KNN<- kmeans(iris_scaled,3)
autoplot(KNN, iris_scaled, frame = TRUE)


