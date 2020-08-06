forest_fires<-read.csv(file.choose())
View(forest_fires)
mydata<-forest_fires[3:31]
View(mydata)
#divide into train and test data
forest_fires_train<-mydata[1:450,]
forest_fires_test<-mydata[451:517,]
##Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
#building model
forest_fires_classifier<-ksvm(forest_fires_train$area~.,data=forest_fires_train,kernel="vanilladot")
forest_fires_classifier
## Evaluating model performance ----
# predictions on testing dataset
forest_fires_predictions<-predict(forest_fires_classifier,forest_fires_test)
table(forest_fires_predictions,forest_fires_test$area)
agreement<-forest_fires_predictions==forest_fires_test$area
table(agreement)
prop.table(table(agreement))
## Improving model performance 
model_rbf<-ksvm(forest_fires_train$area~.,data=forest_fires_train,kernel="rbfdot")
pred_rbf<-predict(model_rbf,newdata=forest_fires_test)
mean(pred_rbf==forest_fires_test$area)
table(pred_rbf)
prop.table(table(pred_rbf))

#kernel=vanilladot
model_vanilla<-ksvm(forest_fires_train$area~.,data=forest_fires_train,kernel="vanilladot")
pred_vanilladot<-predict(model_vanilla,newdata=forest_fires_test)
mean(pred_vanilladot==forest_fires_test$area)
