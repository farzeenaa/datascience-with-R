#Import Data:
cigarette_consumption<-read.csv("C:\\Users\\Shaziya A\\Downloads\\Cigarttes.csv")

#Scatter Plot Matrix:
pairs(cigarette_consumption[,2:8])
#Correlation  
#Correlation Matrix:
cor(cigarette_consumption[,2:8])

#Regression Model:
reg.model<-lm(Sales~Age+HS+Income+Black+Female+Price,data = cigarette_consumption)
summary(reg.model)
