# Using Random Forest
install.packages("randomForest")
library(randomForest)
data(iris)
View(iris)
head(iris)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
rf <- randomForest(Species~., data=iris_train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
attributes(rf)
# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
head(pred_train)
library(caret)

# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 

# Confusion Matrix 
confusionMatrix(iris_test$Species, pred_test)
# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(iris_train[,-5], iris_train[,5], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Species~., data=iris_train, ntree = 140, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, iris_train)
confusionMatrix(pred1, iris_train$Species) 
# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, iris_test)
confusionMatrix(pred2, iris_test$Species) 
# Variable Importance :

varImpPlot(rf1)
# Quantitative values 
importance(rf1)
