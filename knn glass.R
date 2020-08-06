library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)
glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
# table or propagation entries
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("Al","Na","Mg")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_norm <- as.data.frame(lapply(glass[1:9], norm))
View(glass_norm)
anyNA(glass_norm)
glass$Type<-as.factor(glass$Type)
anyNA(glass$Type)

#create training and test datasets
glass_train<-glass_norm[1:150,]
glass_test<-glass_norm[151:214,]

#Get labels for training and test datasets
glass_train_labels<-glass[1:150,10]
glass_test_labels<-glass[151:214,10]
anyNA(glass_train)
anyNA(glass_test)
na.omit(glass_train_labels)
dim(glass_train)
length(glass_train_labels)
# Build a KNN model on taining dataset
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
library(class)
glass_pred<- knn(train =glass_train,test =glass_test,cl= glass_train_labels,k=1)
glass_pred
glass_test_labels

#error in prediction
error<-mean(train=glass_pred!=glass_test_labels)
table(glass_pred,glass_test_labels)
#confusion matrix
confusionMatrix(glass_pred,glass_test_labels)
#iterations for k values
glass_pred<-NULL
error.rate<-NULL
for(i in 1:10){
  glass_pred<-knn(glass_train,glass_test,glass_train_labels,k=i)
error.rate[i]<-mean(glass_pred!=glass_test_labels)  
}
knn.error<-as.data.frame(cbind(k=1:10,error.type=error.rate))
#plotting k value vs error choose k value
ggplot(knn.error,aes(k,error.type))+geom_point()+geom_line()+
  scale_x_continuous(breaks = 1:10)+theme_bw()+xlab("value of k")+ylab("error")

#for k=3
glass_pred<-knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=3)
glass_pred
glass_test_labels

#error in prediction
error<-mean(glass_pred!=glass_test_labels)
table(glass_pred,glass_test_labels)
#confusion matrix
confusionMatrix(glass_pred,glass_test_labels)

#normalizing data
standard.features<-scale(glass[,1:9])
#joining the standardized data with the target column
data<-cbind(standard.features,glass[10])
anyNA(data)
set.seed(101)
sample<-sample.split(data$Type,SplitRatio = 0.70)
train<-subset(data,sample==TRUE)
test<-subset(data,sample==FALSE)

#knn  model building with random sampling
glass_pred_rnd<-knn(train[1:9],test[1:9],train$Type,k=1)

#error in prediction
error<-mean(glass_pred_rnd!=test$Type)
#confusion matrix
confusionMatrix(glass_pred_rnd,test$Type)

#iterations for k values
glass_pred_rnd<-NULL
error.rate<-NULL
for(i in 1:10){
  glass_pred_rnd<-knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i]<-mean(glass_pred_rnd!=test$Type)  
}
knn.error1<-as.data.frame(cbind(k=1:10,error.type=error.rate))
#plotting k value vs error choose k value
ggplot(knn.error1,aes(k,error.type))+geom_point()+geom_line()+
  scale_x_continuous(breaks = 1:10)+theme_bw()+xlab("value of k")+ylab("error")

#knn model building with random sampling
glass_pred_rnd<-knn(train[1:9],test[1:9],train$Type,k=3)
error<-mean(glass_pred_rnd!=test$Type)
confusionMatrix(glass_pred_rnd,test$Type)
