library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
forestfires<-read.csv(file.choose())
View(forestfires)
class(forestfires)
str(forestfires)
ff<-forestfires[,1:11]
View(ff)
#converting month and day string variables into numeric values
ff$month<-as.numeric(as.factor(ff$month))
ff$day<-as.numeric(as.factor(ff$day))
ff1<-mutate(ff,y=log(area+1))
summary(ff1)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
ff_norm<-as.data.frame(lapply(ff1,FUN=normalize))
summary(ff_norm)
#data partition
ind<-sample(2,nrow(ff_norm),replace = TRUE,prob=c(0.7,0.3))
ff_train<-ff_norm[ind==1,]
ff_test<-ff_norm[ind==2,]
#building model
ff_model<-neuralnet(area~.,data = ff_train)
str(ff_model)
plot(ff_model,rep="best")
summary(ff_model)
#improving model performance
set.seed(12323)
model_results<-compute(ff_model,ff_test)
str(model_results)
predicted_strength<-model_results$net.result
cor(predicted_strength,ff_test$area)
plot(predicted_strength,ff_test$area)
mean(predicted_strength==ff_test$area)

ff_model2<-neuralnet(area~.,data = ff_train,hidden=5,linear.output = T)
plot(ff_model2)
model2_results<-compute(ff_model2,ff_test)
model2_results$net.result
str(model2_results)
predicted_strength2<-model2_results$net.result
cor(predicted_strength2,ff_test$area)
plot(predicted_strength2,ff_test$area)
mean(predicted_strength2==ff_test$area)
