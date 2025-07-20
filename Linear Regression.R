
mouse.data<- data.frame(weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),
                        size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3))
mouse.data
plot(mouse.data$weight, mouse.data$size)

regression.mouse<- lm( size~weight,data = mouse.data )

summary(regression.mouse)

abline(regression.mouse, col="navy")


data(Orange)

plot(Orange$age, Orange$circumference)

orange.regression<- lm(circumference~age, data = Orange)
summary(orange.regression)
 abline(orange.regression, col= "orange")
 
 
 predicted_values<- predict(orange.regression)
head(predicted_values) 

cbind(actual= Orange$circumference, predicted=predicted_values) 
 
 
new_data <- data.frame(age = c(1200, 1500))
predicted_new <- predict(orange.regression, newdata = new_data)
predicted_new



 
 
 
 
 
 