library(readr)
calories_consumed<-read.csv(file.choose())
View(calories_consumed)
attach(calories_consumed)
plot(calories_consumed$Weight.gained..grams.,calories_consumed$Calories.Consumed)
summary(calories_consumed)
#Correlation Coefficient (r)
cor(Weight.gained..grams.,Calories.Consumed)  
#simple linear regression model
reg<-lm(Calories.Consumed~Weight.gained..grams.)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(calories_consumed))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = "predict")

# ggplot for adding regresion line for data
#visualisation plot
library(ggplot2)
?ggplot
ggplot(data = calories_consumed, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =calories_consumed, aes(x=Calories.Consumed, y=Weight.gained..grams.))
