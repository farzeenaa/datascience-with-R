startup_50<-read.csv(file.choose())
View(startup_50)
summary(startup_50)
pairs(startup_50)
cor(startup_50)
startup=startup_50[,-4]
View(startup)
pairs(startup)
cor(startup)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup))
# The Linear Model of interest with all the columns
model.startup <- lm(Profit~.,data=startup)
summary(model.startup)
# Multicollinearity check
#model based on only administration
model.A<-lm(Profit~Administration, data=startup)
summary(model.A)

#model based only on marketing spend
model.MS<-lm(Profit~Marketing.Spend, data = startup)
summary(model.MS)

# model based on administartion and marketing.spend
model.AM<-lm(Profit~Administration+Marketing.Spend, data = startup)
summary(model.AM)

library(car)
vif(model.startup)# Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startup,id.n=2,id.cex=0.7)

# Deletion Diagnostics for identifying influential observations
influence.measures(model.startup)

## plotting Influential measures 
influenceIndexPlot(model.startup,id.n=3) # index plots for infuence measures
influencePlot(model.startup,id.n=3) # A user friendly representation of the above

# Regression after deleting the 46,47th  observation, which is influential observation
model_1<-lm(Profit~.,data=startup[-c(46,50),])
summary(model_1)


## Final model
plot(lm(Profit~.,data=startup[-c(49),]))
summary(lm(Profit~.,data=startup[-c(49),]))
plot(lm(Profit~.,data=startup[-c(47,49),]))
summary(lm(Profit~.,data=startup[-c(47,49),]))
plot(lm(Profit~.,data=startup[-c(47,49,50),]))
summary(lm(Profit~.,data=startup[-c(47,49,50),]))

# Its not a feasible solution if we remove all the 
# influential values 
# We need to consider other assumptions to likes
# Heteroscadasticity | Normal Distribution of Residuals


finalmodel<-lm(Profit~.,data=startup[-c(47,49),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution

# simple linear regression
model_2<-lm(Profit~sqrt(R.D.Spend)+sqrt(Administration)+sqrt(Marketing.Spend),data = startup)
summary(model_2)







