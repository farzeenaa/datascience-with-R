claimants<-read.csv("C:\\Users\\Shaziya A\\Downloads\\claimants.csv")

# Linear Regression

fit=lm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT)	
       + CLMAGE + LOSS,data=claimants)
       summary(fit)

plot(fit)
# Logistic Regression

logit=glm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT) 
          + CLMAGE + LOSS,family= "binomial",data=claimants)
summary(logit)

# Odds Ratio

exp(coef(logit))

# Confusion Matrix Table

prob=predict(logit,type=c("response"),claimants)

confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion
# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#plot(rocperf)
install.packages(" AUC")
auc <- auc(claimants$ATTORNEY ~ prob)
auc










