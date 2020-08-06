library(readxl)
Airlines<-read_excel(file.choose())
View(Airlines)
plot(Airlines$Passengers,type="l")
#as the data is for 12months of the year , we create 12 dummy variable
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
Airlines<-cbind(Airlines,X)
View(Airlines)
Airlines["t"]<-1:96
View(Airlines)
Airlines$log_passengers=log(Airlines$Passengers)
head(Airlines)
#Airlines["log_passengers"]<-log(Airlines["passengers"])
Airlines["t_square"]<-Airlines["t"]*Airlines["t"]

##Data Partition
train<-Airlines[1:84,]
test<-Airlines[85:96,]
library(Metrics)
###linearmodel##
linear_model<-lm(Passengers~t,data = train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Passengers,linear_pred$fit)
rmse_linear

##exponential
expo_model<-lm(log_passengers~t,data=train)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Passengers,exp(expo_pred$fit))
rmse_expo

##Quadratic
Quad_model<-lm(Passengers~t+t_square,data=train)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Passengers,Quad_pred$fit)
rmse_Quad 

##Additive Seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

## Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

## Additive Seasonality with Quadratic

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

## Multiplicative Seasonality
multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

## Multiplicative Seasonality Linear trend
multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#building a model
new_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data =Airlines)
View(new_model)
summary(new_model)
predict(new_model,na.head=1)
# Getting residuals 
resid <- residuals(new_model)
acf(resid,lag.max = 12)

?arima
#arima
# Building Autoregressive model on residuals consider lag-1 
k <- arima(resid, order=c(1,0,0))
windows();acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred

####################### Predicting new data #############################

pred_new<-data.frame(predict(new_model,data=Airlines,interval = 'predict'))
View(pred_new)
new_model_fin<-exp(new_model$fitted.values)
View(new_model_fin)

final<-as.data.frame(cbind("Month",Airlines$Passengers,new_model_fin))
colnames(final) <-c("Month","Passengers","New_Pred_Value")

final<-as.data.frame(final)
View(final)


