library(pROC)
library(randomForest)
set.seed(420)

num.samples<-100
weight<- sort(rnorm(n=num.samples,mean = 172, sd=29))

obese<- ifelse(test = (runif(n=num.samples)< (rank(weight)/100)), yes = 1, no=0)
obese

plot(x= weight, y=obese)

glm.fit=glm(obese ~ weight, family = binomial)
lines(weight,glm.fit$fitted.values)

rf.model<- randomForest(factor(obese) ~ weight)

roc(obese, glm.fit$fitted.values, plot=TRUE)
par(pty="s")
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes = TRUE, percent = TRUE,
xlab = "False Positive Percentage",ylab = "True Positive Perccentage", col = "#377eb8", lwd=4, print.auc = TRUE)

plot.roc(obese, rf.model$votes[,1], percent = TRUE, col = "#4daf4a", lwd=4, print.auc =TRUE, add= TRUE, print.auc.y = 40)

roc.info<- roc(obese, glm.fit$fitted.values, legacy.axes = TRUE)

roc.df<- data.frame(tpp= roc.info$sensitivities*100,
                    fpp=(1-roc.info$specificities)*100,
                    thresholds= roc.info$thresholds)
tail(roc.df)

roc.df[roc.df$tpp>60 & roc.df$tpp<80,]
par(pty="m")



