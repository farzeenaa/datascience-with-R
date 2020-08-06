library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)
# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model
Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)
confusionMatrix(Model_pred,test_sal$Salary)
accuracy1<-mean(Model_pred==test_sal$Salary)
accuracy1

model2<-naiveBayes(train_sal$Salary ~ ., data = train_sal,laplace=2)
model2
Model_pred_2<- predict(model2,test_sal)
mean(Model_pred_2==test_sal$Salary)
confusionMatrix(Model_pred_2,test_sal$Salary)
accuracy2<-mean(Model_pred_2==test_sal$Salary)
accuracy2

