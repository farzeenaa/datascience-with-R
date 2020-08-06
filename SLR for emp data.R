library(readr)
emp_data <- read_csv(file.choose())
View(emp_data)
emp_data
# Exploratory data analysis
summary(emp_data)
#Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y)
?plot
attach(emp_data)
#Correlation Coefficient (r)
cor(Salary_hike , Churn_out_rate)             # cor(X,Y)
# Simple Linear Regression model
reg <- lm(Churn_out_rate~Salary_hike) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)
?ggplot2
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))

# Logrithamic Model
# x = log(salary hike); y = churn out rate
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
######################
# Exponential Model
# x = salary hike and y = log(churn out rate)
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)
reg_exp$residuals


sqrt(mean(reg_exp$residuals^2))

loged <- predict(reg_exp)
ed <- exp(loged)

error = emp_data$Churn_out_rate - ed
error

sqrt(sum(error^2)/nrow(emp_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike , y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))

