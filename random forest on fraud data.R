# Using Random Forest
install.packages("randomForest")
library(randomForest)
fraudcheck<-read.csv(file.choose())
View(fraudcheck)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
risky_good<-ifelse(fraudcheck$Taxable.Income<=3000,"Risky","Good")
FC=data.frame(fraudcheck,risky_good)
View(FC)
FC_train<-FC[1:300,]
FC_test<-FC[301:600,]

# Building a random forest model on training data 
fit.forest <- randomForest(risky_good~.,data=FC_train, na.action=na.roughfix,importance=TRUE)

# Training accuracy 
mean(FC_train$risky_good==predict(fit.forest,FC_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,train)
library(caret)

# Confusion Matrix
confusionMatrix(train$Sales, pred_train)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=test)
mean(pred_test==FC_test$risky_good) # Accuracy = 100% 


# Confusion Matrix 

confusionMatrix(FC_test$risky_good, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:6,cex=0.8,fill=1:6)
