install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
library(caret)
library(gmodels)
fraudcheck<-read.csv(file.choose())
View(fraudcheck)
str(fraudcheck)
#splitting data
risky_good=ifelse(fraudcheck$Taxable.Income<=30000,"Risky","Good")
FC=data.frame(fraudcheck,risky_good)
View(FC)
FC_train<-FC[1:300,]
FC_test<-FC[301:600,]
# model building
income_train<-C5.0(risky_good~.,data = FC_train)
windows()
plot(income_train)
pred_train<-predict(income_train,FC_train)
length(pred_train)
FC_train
mean(FC_train$risky_good==pred_train)
confusionMatrix(pred_train,FC_train$risky_good)
pred_test<-predict(income_train,newdata=FC_test)
mean(pred_test==FC_test$risky_good)
confusionMatrix(pred_test,FC_test$risky_good)
CrossTable(FC_test$risky_good,pred_test)
