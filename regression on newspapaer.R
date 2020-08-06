#Load data
Nd<-read.csv("/Volumes/Data/Course Content/DS content/Linear Regression/NewspaperData.csv")

# Visualization
install.packages("lattice")
library(lattice)
dotplot(WC_AT$AT, main="Dot Plot of Sunday Circulations",col="dodgerblue4")
dotplot(WC_AT$Waist, main="Dot Plot of Daily Circulations", col="dodgerblue4")
boxplot(WC_AT$AT,col="dodgerblue4")
boxplot(WC_AT$Waist,col="dodgerblue4")

#Regression equation
#Syntax  model<-lm(y~x,data=data set name)
colnames(WC_AT)
model<- lm(AT~Waist,data =WC_AT)
summary(model)

newdata=data.frame(Waist=c(40,70,100))
AT=predict(model,newdata=newdata)
AT


