library(readr)
delivery_time <- read_csv(file.choose())
View(delivery_time)
delivery_time
# Exploratory data analysis
summary(delivery_time)
#Scatter plot
plot(delivery_time$`Sorting Time`, delivery_time$``)  # plot(X,Y)
attach(delivery_time)
#Correlation Coefficient (r)
cor(`Sorting Time`, `Delivery Time`)             # cor(X,Y)
# Simple Linear Regression model
reg <- lm(`Delivery Time` ~ `Sorting Time`) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(delivery_time))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)
?ggplot2
ggplot(data = delivery_time, aes(x = `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=`Sorting Time`, y=pred))

# Logrithamic Model
# x = log(sorting time); y = deliver time
plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)
reg_log <- lm(`Delivery Time` ~ log(`Sorting Time`))   # lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(delivery_time))  #RMSE
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
# Exponential Model
# x =sorting time and y = log(delivery time)
plot(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`, log(`Delivery Time`))
reg_exp <- lm(log(`Delivery Time`) ~ `Sorting Time`)  #lm(log(Y) ~ X)

summary(reg_exp)
reg_exp$residuals


sqrt(mean(reg_exp$residuals^2))

logdt <- predict(reg_exp)
dt <- exp(logdt)

error = delivery_time$`Delivery Time` - dt
error

sqrt(sum(error^2)/nrow(delivery_time))  #RMSE
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`*`Sorting Time`, `Delivery Time`)

cor(`Sorting Time`*`Sorting Time`,`Delivery Time`)

plot(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm((`Delivery Time`) ~ `Sorting Time` + I(`Delivery Time`*`Delivery Time`))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time$`Delivery Time` - expy

sqrt(sum(err^2)/nrow(delivery_time))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = delivery_time, aes(x = `Sorting Time` + I(`Sorting Time`^2), y = log(`Delivery Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=`Sorting Time`+I(`Sorting Time`^2), y=logpol))
