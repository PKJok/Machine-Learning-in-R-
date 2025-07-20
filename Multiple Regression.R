data(chickwts)
plot(chickwts$feed, chickwts$weight)

mouse.data<- data.frame(weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),
                        size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3),
                        tail= c(0.7,1.3,0.7,2.0,3.6,3.0,2.9,3.9,4.0))
mouse.data

plot(mouse.data$weight, mouse.data$size)
 sample.regression <- lm(size~weight, data = mouse.data)
summary(sample.regression) 

abline(sample.regression, col="red", lwd=3)

# now multiple regression

plot(mouse.data)

multiple.regression <- lm(size ~ weight + tail, data = mouse.data)
summary(multiple.regression)
