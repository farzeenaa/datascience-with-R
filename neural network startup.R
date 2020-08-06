library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
startups<-read.csv(file.choose())
View(startups)
class(startups)
startups$State<-as.numeric(revalue(startups$State,c("New York"="0",California="1","Florida"="2")))
str(startups)
startups<-as.data.frame(startups)
attach(startups)
windows()
pairs(startups)
cor(startups)
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
startups_norm<-as.data.frame(lapply(startups, FUN = normalize))
summary(startups_norm$Profit)
#data partition
ind<-sample(2,nrow(startups_norm),replace = TRUE,prob=c(0.7,0.3))
startups_train<-startups_norm[ind==1,]
startups_test<-startups_norm[ind==2,]
#buildind model
startups_model<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startups_train)
str(startups_model)
plot(startups_model)
summary(startups_model)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
str(model_results)
predicted_profit <- model_results$net.result
cor(predicted_profit,startups_test$Profit)
#since the data is in normalized form,we need to denormalize it to get actual prediction on profit
str_max<-max(startups$Profit)
str_min<-min(startups$Profit)
unnormalize<-function(x,min,max){
  return(max-min)*x+min
}
actualprofit_pred<-unnormalize(predicted_profit,str_max,str_min)
#improve model performance
set.seed(12324)
startups_model2<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startups_train,hidden = 2)
str(startups_model2)
plot(startups_model2)
summary(startups_model2)

model_results2 <- compute(startups_model2, startups_test[1:4])
predicted_profit2 <- model_results2$net.result
cor(predicted_profit2, startups_test$strength)
