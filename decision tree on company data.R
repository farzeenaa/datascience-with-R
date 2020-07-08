install.packages("caret")
install.packages("C50")
library(caret)
library(C50)
install.packages("tree")
company_data<-read.csv(file.choose())
View(company_data)
colnames(company_data)
company_data$Sales<-ifelse(company_data$Sales<=median(company_data$Sales),"Low","High")
company_data$Sales<-factor(company_data$Sales)
View(company_data)
company_data<-na.omit(company_data)
set.seed(111)
split<-createDataPartition(y=company_data$Sales,p=0.6,list = FALSE)
train<-company_data[split,]
test<-company_data[-split,]

#building model on training data
sales_train<-C5.0(Sales~.,data=train)
window()
plot(sales_train)
#training  accuracy
pred_train<-predict(sales_train,train)
mean(train$Sales==pred_train)
confusionMatrix(pred_train,train$Sales)
pred_test<-predict(sales_train,newdata=test)
mean(pred_test==test$Sales)
library(gmodels)
# Cross tables
CrossTable(test$Sales,pred_test)
#cross validating
set.seed(12)
library(tree)
tree<-tree(Sales~.,data=train)
cv_sales_tree<-cv.tree(tree,FUN = prune.misclass)
plot(cv_sales_tree)

prune_sales_tree<-prune.misclass(tree,best=4)
prune.pred<-predict(prune_sales_tree,test,type="class")

plot(prune_sales_tree)
text(prune_sales_tree,pretty=0)
confusionMatrix(prune.pred,test$Sales)
