MPG<-read.csv("C:\\Users\\Shaziya A\\Downloads\\Cars.csv")

#Scatter Plot Matrix:
pairs(MPG)


#Correlation Matrix:
cor(MPG)
#Regression Model and Summary
model.car<-lm(MPG~HP+VOL+SP+WT,data = MPG)
summary(model.car)
#########Experiment#####################
reg_vol<-lm(MPG~VOL,data = MPG)
summary(reg_vol)
reg_wt<-lm(MPG~WT,data = MPG)
summary(reg_wt)
reg_wt_vol<-lm(MPG~WT+VOL,data = MPG)
summary(reg_wt_vol)
##################
#Regression Model and Summary
model.car<-lm(MPG~.,data = MPG)
summary(model.car)
#Multi-colinearity
install.packages("car")
library(car)
car::vif(model.car)
##Subset selection
library(MASS)
stepAIC(model.car)
#Model Building
#Regression Model and Summary
model.car<-lm(MPG~.,data = MPG)
summary(model.car)
#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.car) 
#Residuals vs Regressors
library(car)
residualPlots(model.car)
#Added Variable Plots
avPlots(model.car)
#QQ plots of studentized residuals
qqPlot(model.car)
#Deletion Diagnostics
influenceIndexPlot(model.car) # Index Plots of the influence measures

####Iteration 1 
#Remove 77th observation
MPG1<-MPG[-77,]
model1<-lm(MPG~.,data = MPG1)
car::vif(model1)
plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

#iteration2
MPG2<-MPG[-c(77,79,66),]

