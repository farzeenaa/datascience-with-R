#SVM on salary data
library(kernlab)
#data on train
train_sal<-read.csv(file.choose())
str(train_sal)
View(train_sal)
class(train_sal)
#data on test
test_sal<-read.csv(file.choose())
str(test_sal)
View(test_sal)
class(test_sal)
#building model
model1<-ksvm(train_sal$Salary~.,data=train_sal,kernel="vanilladot")
model1
#evaluting model
salary_prediction<-predict(model1,test_sal)
table(salary_prediction,test_sal$Salary)
agreement<-salary_prediction==test_sal$Salary
table(agreement)
prop.table(table(agreement))
#kernel=rfdot
model_rbf<-ksvm(train_sal$Salary~.,data=train_sal,kernel="rbfdot")
pred_rbf<-predict(model_rbf,newdata=test_sal)
mean(pred_rbf==test_sal$Salary)
