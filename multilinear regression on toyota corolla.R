toyota<-read.csv(file.choose())
View(toyota)
summary(toyota)
corolla<-cbind()
toyota$KM


Corolla<-toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
Corolla1<-as.data.frame(Corolla)
class(Corolla1)
View(Corolla1)
summary(Corolla1)
pairs(Corolla1)
cor(Corolla1)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla1))
# The Linear Model of interest with all the columns
model.corolla <- lm(Price~ Age_08_04,data=Corolla1)

summary(model.corolla)

model.corollapakh<-lm(Price~Age_08_04+KM+HP,data=Corolla1)
summary(model.corollapakh)

model.corollapakhcdg<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears,data = Corolla1)
summary(model.corollapakhcdg)

model.corollac<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla1)
summary(model.corollac)

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
influence.measures(model.corollac)
library(car)
vif(model.corollac) # Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.corollac,id.n=2,id.cex=0.7)
## plotting Influential measures 
influenceIndexPlot(model.corollac,id.n=3) # index plots for infuence measures
influencePlot(model.corollac,id.n=3) # A user friendly representation of the above

# Regression after deleting the 81th observation, which is influential observation
model_1<-lm(Price~.,data=Corolla[-81,])
summary(model_1)

# Regression after deleting the 81,222th & 961 Observations
model_2<-lm(Price~.,data=Corolla[-c(81,222,961),])
summary(model_2)

## Final model
plot(lm(Price~.,data=Corolla[-c(81),])) 
summary(lm(Price~.,data=corolla[-c(81),]))
plot(lm(price~.,data=Corolla[-c(81,222),])) 
summary(lm(Price~.,data=Corolla[-c(81,222),]))
plot(lm(Price~.,data=corolla[-c(81,222,961),]))
summary(lm(Price~.,data=Corolla[-c(81,222,961),]))

finalmodel<-lm(Price~.,data=Corolla[-c(81,222),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution




