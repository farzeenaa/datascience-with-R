library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)
zoo <- read.csv(file.choose())
View(zoo)
table(zoo$type)
table(zoo$animal.name)
# table or propagation entries
round(prop.table(table(zoo$Type))*100,1)
zoo1<-zoo[,2:18]
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
zoo_norm <- as.data.frame(lapply(zoo[2:17], norm))
View(zoo_norm)
anyNA(zoo_norm)
zoo$type<-as.factor(zoo$type)
anyNA(zoo$type)
str(zoo$type)

#joining the standardized data with the target column
data<-cbind(zoo_norm,zoo[18])
anyNA(data)
set.seed(101)
sample<-sample.split(data,SplitRatio = 0.70)
train<-subset(data,sample==TRUE)
test<-subset(data,sample==FALSE)
dim(train[1:16])
dim(test[1:16])


#knn  model building with random samplin
zoo_pred<-knn(train[1:16],test[1:16],train$type,k=1)

#error in prediction
error<-mean(zoo_pred!=test$type)
zoo_pred
#confusion matrix
confusionMatrix(zoo_pred,test$type)

#iterations for k values
zoo_pred<-NULL
error.rate<-NULL
for(i in 1:10){
  zoo_pred<-knn(train[1:16],test[1:16],train$type,k=i)
  error.rate[i]<-mean(zoo_pred!=test$type)  
}
knn.error1<-as.data.frame(cbind(k=1:10,error.type=error.rate))
#plotting k value vs error choose k value
ggplot(knn.error1,aes(k,error.type))+geom_point()+geom_line()+
  scale_x_continuous(breaks = 1:10)+theme_bw()+xlab("value of k")+ylab("error")

#knn model building with random sampling
zoo_pred2<-knn(train[1:16],test[1:16],train$type,k=3)
error<-mean(zoo_pred2!=test$type)
confusionMatrix(zoo_pred2,test$type)
