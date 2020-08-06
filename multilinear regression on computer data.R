library(readr)
computer_data<-read.csv(file.choose())
View(computer_data)
computer_data1<-computer_data[,-1]
library(plyr)
computer_data1$cd<-as.numeric(revalue(computer_data1$cd,c("yes"=1, "no"=0)))
computer_data1$multi<-as.numeric(revalue(computer_data1$multi,c("yes"=1,"no"=0)))
computer_data1$premium<-as.numeric(revalue(computer_data1$premium,c("yes"=1,"no"=0)))
View(computer_data1)
summary(computer_data1)
cor(computer_data1)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(computer_data1))
# The Linear Model of interest with all the columns
model.computer_data1.ps<-lm(price~speed,data=computer_data1)
summary(model.computer_data1.ps)

model.computer_data1.psh<-lm(price~speed+hd,data = computer_data1)
summary(model.computer_data1.psh)

model.computer_data1.psdr<-lm(price~speed+hd+ram,data = computer_data1)
summary(model.computer_data1.psdr)

model.computer_data1.psdrsc<-lm(price~speed+hd+ram+screen+cd,data = computer_data1)
summary(model.computer_data1.psdrsc)

model.computer_data1.psdrscmp<-lm(price~speed+hd+ram+screen+cd+multi+premium,data = computer_data1)
summary(model.computer_data1.psdrscmp)

model.computer_data1 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = computer_data1)
summary(model.computer_data1)

library(car)
vif(model.computer_data1)# Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.computer_data1,id.n=2,id.cex=0.7)

# Deletion Diagnostics for identifying influential observations
influence.measures(model.computer_data1)

## plotting Influential measures 
influenceIndexPlot(model.computer_data1,id.n=3) # index plots for infuence measures
influencePlot(model.computer_data1,id.n=3) # A user friendly representation of the above

# Regression after deleting the 1441,1701th  observation, which is influential observation
model.computer_data2<-lm(price~.,data=computer_data1[-c(1441,1701),])
summary(model.computer_data2)

# logarthmic transformation
model.computer_datalog<-lm(price~log(speed)+log(hd)+log(ram)+log(screen)+log(cd)+log(multi)+log(multi)+log(premium) + log(ads)+log(trend),data = computer_data1[-c(1441,1701),])
summary(model.computer_datalog)

model.computer_data2<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = computer_data1[-c(1441,1701),])
summary(model.computer_data2)

# exponential transformation
model.computer_dataexp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = computer_data1[-c(1441,1701),])
summary(model.computer_dataexp)

#quad model
model.computer_dataquad<-lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                              cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)+ads+I(ads^2)+
                         trend+I(trend^2),data=computer_data1[-c(1441,1701),] )
summary(model.computer_dataquad)

# poly model
model.computer_datapol<-lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+
                              cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(multi^3)+ads+I(ads^2)+I(ads^3)+
                              trend+I(trend^2)+I(trend^3),data=computer_data1[-c(1441,1701),] )
summary(model.computer_datapol)

finalmodel<-lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(multi^3)+ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=computer_data1[-c(1441,1701),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
hist(residuals(finalmodel)) # close to normal distribution

